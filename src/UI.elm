
module UI exposing
    -- draw uis --
    ( UI
    , view
    
    , ActionRing
    , InteractiveElement
    , Interactivity (..)
    , ring
    , Static 
    , create
    , Role (..)
    , Items, Page, Navigate

    -- intercept stack to annotate views --
    , Indicator
    , asAnnotator )

import Debug

import Tuple exposing ( first, second, mapFirst, mapSecond )
import List exposing ( map )
import Stack exposing ( .. )

import Lazy.Tree as Tree exposing ( Tree )
import Lazy.LList as LL exposing ( LList )
import Lazy.Tree.Zipper as Zipper exposing ( Zipper, root, getTree )

import Html exposing ( Html, label, text, a, p, div, button, span, li, ul, text, section )
import Html.Attributes exposing ( class, href )
import Html.Events exposing ( onClick )
import Html.Extra exposing ( static )

import Result exposing ( Result )

import Array exposing ( Array, length )
import Url exposing (Url)

import Tagged exposing ( tag )




{-
    
    UI
    
    provides defaults and boundaries for SPAs.
    An ui is a Constant that is fed lazy lookup
    functions for any data that may change. So you have to
    be explicit about mutability.

    In the current implementation, prologue and epilogue are
    static HTML.


 -- Semantic Markup

    Html5 introduces semantic elements with precise roles.
    UI provides Roles to markup individual items. Roles do
    not directly translate into Html Elements but are
    interpreted based on context. So depending on where a
    'Title' appears, it can be the h1 of the document, the
    h2 of an article or a h3.. of a nested article.
    Additionally, the role of an item may dictate its
    order and nesting, as in introductions being merged and
    enclosed, with title, into a header:

    -- article -------------
       -- header --------
          title
          ?introductions
          ?(autonav)
       ------------------
       miscs,
       lists
       (in any order)
      ?-- footer --------
          (autometa)
       ------------------
    ------------------------
    


 -- stack and Items
 
    Every item is referred to by its key. A stack of stack can
    be reconstructed using the getPath function.

    An Item is its key.

 
-- Talking to an UI

    All aspects of an ui are either immutable
    or functions. To draw items, provide functions that accept
    stacks of stack. You can manage your own dicts.

    The window and the focus will be evaluated on each view.


 -- Wiring

    In your init, you have to provide all typical navigation
    messages so that the UI can put them into your Html.

    The navigation messages are conceptually separate from the
    application-specific "actions" that you build your buttons
    with, or the hrefs you can use in links.

 -}

type alias Static = List ( Html Never )
type alias Dynamic msg = List ( Html msg ) 

-- get multiple single steps.
type alias Steps state key =
    state -> Stack key

type alias ChildSteps state key =
    state -> Stack key -> List key

type UI state key msg =
    UI ( Items state key msg ) ( Page state key ) ( Navigate state key msg )

type alias Items state key msg = state -> Stack key ->
    { presentation: Static
    , interaction: ActionRing ( Stack key ) msg
    , role: Role
    , childrenWrapper: ( Dynamic msg -> Dynamic msg )
    , childrenKeys: List key
    }
type alias Page state key =
    { prologue: state -> Static
    , epilogue: Static
    , meta: Static
    , window: state -> Stack key
    , focus: state -> List key
    }
type alias Navigate state key msg =
    { focus: Stack key -> msg
    , back: msg
    , link: state -> Stack key -> String
    }

create = UI
 



-- An element that is already assigned a path.
type Item state path msg
    = Interactive ( ActionRing path msg )
    | Passive ( Dynamic msg ) -- forgets its path

-- Rotate with Tab key.
type alias ActionRing path msg =
    Ring ( InteractiveElement path msg )

-- Define an element as the product of one interactivity and one representation.
type alias InteractiveElement path msg =
    { interactivity: Interactivity path msg
    , presentation: Static }

type Interactivity path msg
    = Button msg
    | Link { url: String }
    | Self path
    | Cancel
    | OK
    | Dismiss
    | Back

type Role           
    = Caption
    | ListItem
    | Aside
    | Group
    | Text





------------------------------------------------------------------------------

--The view of an ui only depends on the state.
view : UI state key msg -> state -> { title: String, body: Html msg }
view ( UI itemFunction page navigate ) state =
    let
        items = itemFunction state
        
        -- Evaluate lazy variables
        ( window, steps ) = ( page.window state, page.focus state )

        -- Draw static Html
        viewPrologue = map static ( page.prologue state ) |> section []
        viewEpilogue = map static page.epilogue           |> section []
        viewMeta     = map static page.meta               |> section []

        -- Append each child keys to a copy of this stack
        childPaths : Stack key -> List ( Stack key )
        childPaths = \path -> path |>
            items >> .childrenKeys >> map ( \key -> push key path )

        -- Any passive item doubles as a navigation button to itself
        toPassiveItem : Stack key -> Item state ( Stack key ) msg
        toPassiveItem =
            items >> .presentation >> map static >> Passive
                  
        -- ActionRings are simply wrapped in an Item container
        toInteractiveItem : Stack key -> Item state ( Stack key ) msg
        toInteractiveItem =
            items >> .interaction >> Interactive
            
        -- build the tree from the window, focus, map to items.
        buildWindow : Zipper ( Item state ( Stack key ) msg )
        buildWindow =
            window                                              -- top item
            |> Tree.build childPaths                            -- Tree
            |> Zipper.fromTree                                  -- Zipper ( stack )
            |> Zipper.attemptOpenPath (
                    \s stack ->
                    let t = top stack
                    in case t of
                        Just k -> k == s
                        Nothing -> False
                ) steps     -- walk to focus
            |> distinguishFocus toInteractiveItem toPassiveItem -- evaluate item kind 
         

---------- VIEWING -----------------------------------------------------------

        viewWindow = buildWindow |> Zipper.map viewItem
 
        viewItems = zipperTake maximumRecursionDepth viewWindow |> ul []
        maximumRecursionDepth = 6

        zipperTake max zipper =
            let
                children = if ( max < 0 ) then [] else
                    Zipper.openAll zipper |> List.map ( zipperTake ( max - 1 ) )
                list = List.foldl ( \child acc -> child ++ acc ) [] children
            in
                ( Zipper.current zipper ) ++ ([ span [ class "depth" ][ text ( String.fromInt max ) ]]) ++ list


        viewItem : Item state ( Stack key ) msg -> Dynamic msg
        viewItem item =
            case item of
                Passive content -> [ a [ class "item", href "+"] <| content ]
                Interactive actionRing ->
                    ringList actionRing
                    |> map viewElement
                    |> mapHead ( \x -> div [class "item"] [x] )
    
        mapHead fu li = case li of
                            x::xs -> (fu x) :: xs
                            [] -> []

        viewElement : InteractiveElement ( Stack key ) msg -> Html msg 
        viewElement { interactivity, presentation } =
            case interactivity of
                Button msg ->
                      button  [ onClick msg ] <| map static presentation
                Link { url } ->
                      a       [ href ( url ), class "item" ] <| map static presentation
                Self k ->
                      a      [ class "self", navigate.link state k |> href ]
                             <| [ text "#" ] ++ map static presentation
                Cancel ->
                    label [] <| [ button   [ class "cancel", onClick navigate.back ]
                                           [ text "cancel" ] ] ++ ( map static presentation )
                OK ->       
                    label [] <| [ button   [ class "ok", onClick navigate.back ]
                                           [ text "OK" ] ] ++ ( map static presentation )
                Dismiss ->
                    label [] <| [ button   [ class "dismiss", onClick navigate.back ]
                                           [ text "v" ] ] ++ map static presentation
                Back ->
                    label [] <| [ button   [ class "back", onClick navigate.back ]
                                           [ text "<" ] ] ++ map static presentation

    in
        { title = "UI" 
        , body =
            ul [] <| viewPrologue :: viewItems :: viewEpilogue :: viewMeta :: []
        }














-- Exposed functions

type alias Indicator stack = stack -> String

asAnnotator : Indicator stack -> ( stack -> Static ) -> ( stack -> Static )
asAnnotator indicator drawer
    = ( \stack -> [ span [ class ( indicator stack ) ] ( drawer stack ) ] )
    



------------ STRUCTURES -------------------------------------------------------

        
-- Type helpers

distinguishFocus : ( i -> a ) -> ( i -> a ) -> Zipper i -> Zipper a
distinguishFocus focusfu nofu zipper =
    let newFocusedItem = Zipper.current zipper |> focusfu
    in Zipper.map nofu zipper |> Zipper.updateItem ( always newFocusedItem )


-- Ring is for Tabstops --


type alias Ring a =
    { position: Int
    , slots: Array a
    }


ring : a -> List a -> Ring a
ring primary others =
    { position= 0, slots= Array.fromList <| primary::others }

ringIsPrimary : Ring a -> Bool
ringIsPrimary r = r.position  == 0

ringPrimary r = { r | position = 0 }

ringNext : Ring a -> Ring a
ringNext r =
    { r | position = r.position + 1 |> modBy ( length r.slots )  }

ringPrev : Ring a -> Ring a
ringPrev r =
    { r | position = r.position - 1 |> modBy ( length r.slots ) }

ringCurrent : Ring a -> Maybe a
ringCurrent r =
    Array.get r.position r.slots

ringList : Ring a -> List a
ringList r =
    Array.toList r.slots


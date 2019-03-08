module Form exposing
    ( drawPassive
    , drawInteractive
    , drawChildren
    )

import UI
import Html exposing (..)
import Html.Attributes exposing (..)








{-
    FORM

        TODO:
        We have to remove the multidefinition/Arrangement part and introduce the
        distinction between Signature (unique per type node)
        and Locus (unique per data node).

        Form is the atom of State.


    is the functionality of an Item within an App. 
    It is named with one or more words, or accessed through
    an Edit signature (if it is an appended item).
     

    Structure:

    A concept with multiple definitions is an Arrangement.
        In which case the child items are arranged in a
        horizontal table with a header that connects those
        items that are supposed to go together.
    A concept with only one definition is a Rubric.
        In which case we draw the children as a list.
        The rubric is represented as a sticky, square caption.
    A leaf is always an Input.
        In which case an input controlshould be drawn, with
        the concept name(s) as name.
    A user-appended item is a Variance if it has multiple
    different prototypes.
        In which case we draw the children inside the focused
        element's tab ring, not as items but as 'optional items'.

    
    Besides, we distinguish between unremovable, removable and removed items.
    Structures with unambiguous prorotypes know whether they are
    unappending or appending ones, and in the latter case we
    also ask whether the prototype of appendal is singular or ambiguous.

-}






type Kind

    = Arrangement Name App.Multidefinition
        { this: IfAppended  { choices: Contexts Edit.Choice, removedBy: Contexts () }
        , more: IfAppending ( Contexts Appendal )
        }
    | Rubric Name 
        { this: IfAppended  { choices: Contexts Edit.Choice, removedBy: Contexts () }
        , more: IfAppending ( Contexts Appendal )
        }
    | Input Name
        { this: IfAppended  { choices: Contexts Edit.Choice, removedBy: Contexts () }
        , data: Contexts Edit.Input
        }
    | Variance Name App.Multiprototype
        { removedBy: Contexts () }



type IfAppended a
    = NotAppended
    | Appended a

type IfAppending a
    = NotAppending
    | Appending a

type Appendal
    = Unique App.Prototype
    | Multi App.Multiprototype


type alias Contexts a = 
  List
    { curator: Session.Signature
    , seenBy: List ( Edit.SeenBy )
    , undone: Bool
    , transformation: a
    }







view state path =
    let 
        kind = 
        defaults =
        { presentation = []
        , interaction = ActionRing ( Stack key ) msg
        , role = Role
        , childrenWrapper = ( List Html msg -> List Html msg )
        , childrenKeys = List key
        }
    in case kind of
        Arrangement ->
            
        Rubric ->
            
        Input ->
            
        Variant ->
            
        




drawChildrenWrapper : Kind -> Maybe ( List ( Html msg ) -> Html msg )
drawChildrenWrapper kind =
    let
        removedClass this =
            case this of
                NotAppended -> [ class "notAppended" ]
                Appended { removedBy } -> map ( always <| class "removed" ) removedBy

    in case kind of
        Arrangement name multidefinition { this, more } ->
            Just ul <| [ class "inArrangement" ] ++ ( removedClass this )
        Rubric name { this, more } -> 
            Just ul <| [ class "inRubric" ] ++ ( removedClass this )
        _ -> Nothing

drawPassive : Kind -> ( UI.Static, Maybe UI.Wrapper )
drawPassive kind =
    let
        
    in
     ( case kind of
        Arrangement name multidefinition { this, more } ->
            [ li [ class "arrangement" ] [ text "multidefinition: ", text name ] ]
        Rubric name { this, more } ->
            [ li [ class "rubric" ] [ text "rubric: ", text name ] ]
        Input name ->
            [ label [] [ span [] [ text name ], input [ type_ "text" ] [] ]
        Variance name multiprototype  ->
            [ li [ class "variance" ] [ text name, text " (variance)" ] ]
      , drawChildrenWrapper kind )

drawInteractive : Kind -> ( UI.ActionRing, Maybe UI.Wrapper )
drawInteractive kind =
    let
    
        drawInput name =
            label [] [ span [] [ text name ], input [ type_ "text" ] [] ]
        drawVariance multiprototype =
            name [] [ span [] [ text nameText ], div [] [ text "variance" ] ]
            
    in 
     ( case kind of
        Arrangement name multidefinition { this, more } ->
            ( drawArrangement name ) ++ ( drawPlus more )
        Rubric name { this, more } -> 
            ( drawRubric name ) ++ ( drawPlus more ) 
        Input name ->
            ( drawInput name )
        Variance name multiprototype  ->
            ( drawVariance name ) ++ ( drawMultiprototype multiprototype )
      , drawChildrenWrapper kind
      )



    -- items over keys respond to state.
      drawPassiveItem =    
        (\stack state ->
            [ App.getString stack state.app |> text ] )
    , drawInteractiveItem =
        (\stack state ->
           UI.ring  
            { interactivity =    
                UI.Link { target = "", description = App.getString stack state.app }
            , representation =
                [ text "FOCUS" ] } [] )
    , getChildKeys =
        (\stack state ->
            App.getFirstSteps stack state.app )

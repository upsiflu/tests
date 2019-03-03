module App exposing (App, getFirstSteps, initialApp, wordToStep, getString,
    encodeConcept, encodeApp, Locus, alternativeApp )

import Lazy.LList exposing ( isEmpty, toList )
import Lazy.Tree as Tree exposing ( Tree, build, Forest, item, forestMap, descendants )
import Lazy.Tree.Zipper as Zipper exposing ( Zipper, openAll, current, fromTree, getTree, 
    attemptOpenPath )
import List exposing ( reverse, map, foldl )
import Dict exposing (Dict)

import Stack exposing ( .. )
import String exposing ( join )

import Debug












{-
    
    App
    
    is an immutable structure, written in ambilang,
    known to the computer in the form of a lazy rose-tree.
    
 -- Constellations of an App ------------------------------------
    
    Indentations denote parent-child relation,
    Symbols +, <, >, : denote mappings over children;
    any other word denotes a concept, whereby nested concepts
    are compressed to one node.
    
    A Group may be tagged Definition or Parameters if it
    appears below a Concept resp. a Relation.
    An empty group is always an Input for runtime data.
    
 -- Explicit Ambiguity ------------------------------------------
    
    You can have multiple group members with the
    same name but differing children:
    
    - A Concept defined with variated Definitions yields
      Variants which may be traversed through a Perspective.

    - A Relation defined with alternating Parameters
      creates Alternatives, giving you a Choice.

 -}

 
type alias App
    = Zipper Word

type alias Group
    = Forest Word
 
type alias Step
    = String

type alias Locus
    = Stack Step

getFirstSteps : Locus -> App -> List Step
getFirstSteps l =
    getApp l >> openAll >> map current >> map wordToStep 
        
getString : Locus -> App -> String
getString l =
    getApp l >> current >> wordToStep 

getApp : Locus -> App -> App
getApp l =
    attemptOpenPath ( \step word -> wordToStep word == step ) ( reverse l )
 


------------------------------------------------------

getFakeData : Locus -> App -> String
getFakeData l = getApp l >> current >> wordToStep

type FakeApp = FakeApp ( Word, List FakeApp )
fakeApp : FakeApp
fakeApp =
    FakeApp ( Naming ( Concept "Blog"),
        [ FakeApp ( Naming ( Concept "Title"), [] )
        , FakeApp ( Naming ( Concept "Introduction Paragraph"), [] )
        , FakeApp ( Naming ( Concept "My Contact"), [] )
        , FakeApp ( Naming ( Concept "Posts"), 
            [ FakeApp ( Naming ( Concept "Post 1"), [] )
            , FakeApp ( Naming ( Concept "Post 2"), [] )
            ] ) 
        , FakeApp ( Naming ( Concept "Footer Paragraph"), [] ) 
        ] 
    )

initialApp : App
initialApp = fakeApp |> build getChildren |> fromTree |> Zipper.map getWord
getChildren ( FakeApp ( w, c ) ) = reverse c
getWord ( FakeApp ( w, c ) ) = w
    
--------------------------------------------------------------

    
    
type Word
    = Symbolizing Relation
    | Naming Concept

type Relation
    = More

type Concept
    = Concept String
  
type alias Tocus = List Word

neutral : Tocus
neutral = []



        
        
        
        




{----------------------------------------------------------------
    
    Encoding Structural Ambiguities
    
    Of the five ambiguities that an app can represent
    (see Locus.elm), two can be written in Ambilang and
    are thus explicit and immutable part of an app.
    
    Since words at a given level are unique, you can denote
    ambiguity by repeating a word several times at one level.
    
 -- Multidefinition ---------------------------------------------
    
    A filter is one of several Definitions given for a
    unique Concept. Multiple filters form a kind of table
    where each column only shows data that matches.
    
 -- Multiprototype ----------------------------------------------
    
    If there are parallel prototypes, an item may be
    Item.Ambiguous.

 -- Combining Filters and Prototypes ----------------------------

    If a Concept F, defined a both by F0 and F1, is one
    of the Prototypes of P, nothing surprising happens.

    Conversely, if F is designed such that it can append
    a prototype P0 and a prototype P1, then P0 and P1 will
    only show up in those columns that match their type, i.e.
    exactly "+ P0" resp. "+ P1", but not e.g. "P0".

 ----------------------------------------------------------------}



type Definition = D Group
type Multidefinition = DD ( Multiple Group )
    
type Prototype = P Group
type Multiprototype = PP ( Multiple Group )


type Multiple a
    = Two  a a
    | Many a (Multiple a)
    
    
   



-- ENCODING --
    
encodeConcept ( Concept string ) = string
decodeConcept ( string ) = Concept string

wordToStep : Word -> String
wordToStep word =
    case word of
         Symbolizing More -> "+"
         Naming (Concept string) -> string
         
stepToWord step =
    case step of
        "+" -> Symbolizing More
        string -> Naming (Concept string)


encodeApp : App -> String
encodeApp = getTree >> Tree.map wordToStep >> serialize 0 >> String.dropLeft 1

serialize : Int -> Tree String -> String
serialize depth tree =
    "\n" ++ ( String.repeat depth " " ) ++ ( item tree ) ++
        if descendants tree |> isEmpty then ""
        else descendants tree |> toList |> reverse |> map ( serialize ( depth+1 ) ) |> join ""







{--

Tree.build
.children


NO, how is it really?

We start with the lines. We have to put multidefi/multiproto into a structure before it's evaluated.
We want to be able to add and remove lines.
So we need a structure more akin to a red/black tree, i.e. dict.

add =
Empty -> Insert
Single -> make Ambi
Ambi -> add Ambi

remove =
Empty -> Identity
Single -> Remove
Ambi ->
    2 -> Single
   >2 -> Ambi

Problem: then we have to maintain both a redblack tree  (dict) and an ambi tree (zipper word)?

Yet another option:


    

--}


-- Multis are merged when walking down.
walk steps =
    Zipper.attemptOpenPath
        ( \s stack ->
            case top stack of
                Just t -> t == s
                Nothing -> False
        ) steps

-- Multis are deleted as a whole.
removeLeaf locus =
    walk locus
    >> ( \z -> if isEmpty z then delete z else z )
    >> root



-- here is how to preserve multis: rename every new occurrence by adding a prime to the end.
introduce locus newWord z =
    if open newWord z == Nothing
    then insert newWord z
    else introduce locus ( newWord++"'" ) z )

add locus newWord  =
    walk locus >> introduce locus newWord


{ word: Word, children: List FakeApp }



swallowLine : String -> ( Dict Int ( Tree String ) ) -> ( Dict Int ( Tree String ) )
swallowLine line dict =
    let
        getDepth d rest =
            case String.uncons rest of
                Just (' ', rrest) -> getDepth ( d+1 ) rrest
                _ -> d
        depth = getDepth 0 line |> Debug.log "depth"
        parent = Dict.get (depth-1) dict |> Debug.log "parent"
        newTree = String.dropLeft depth line |> Debug.log "new" |> Tree.singleton
    in
        -- occupy this depth level
        dict |> Dict.insert depth newTree |> Debug.log "swallowing..." |>
        case parent of
            -- the correct parent is extended
            Just pTree -> Dict.insert (depth-1) (Tree.insert newTree pTree)
            -- the parent is updated in the grandparent
                    |> updateGenetics depth
            Nothing -> identity
                            

decodeApp : String -> App
decodeApp =
    String.lines
    >> foldl swallowLine ( Dict.empty )
    >> Debug.log "dict"
    >> Dict.get 0
    >> Maybe.map ( Tree.map stepToWord >> fromTree )
    >> Maybe.withDefault initialApp



-- new: swallow line produces FakeApp, i.e. { item, children }.


serial ="""Blog
 Title
 Introduction Paragraph
 Contact
 Posts
  Post A
  Post B
 Footer Paragraph"""

alternativeApp = decodeApp serial

module Stack exposing ( .. )

import List exposing (..)
import Maybe exposing (..)


type alias Stack a = List a

push : a -> Stack a -> Stack a
push k stack =
    k::stack

pop : Stack a -> Stack a
pop stack =
    case stack of
        s::tack -> tack
        [] -> []

top : Stack a -> Maybe a
top stack = 
    case stack of
        s::tack -> Just s
        [] -> Nothing

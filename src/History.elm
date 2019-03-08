module History
import Transformation


{-    
    History

    This structure caches the latest State of the application.
    It can be fed with Transformations which will be inserted
    in alphanumeric order.

    History enables collaboration over unreliable networks:
    Instances that receive the same Transformations in any order
    eventually synch.

    History is a zipper; it represents a hole in a stack.
    A Transformation can be inverted. History stores
    positive transformations to the top of the current state
    and inverted ones towards the bottom.

        singleton State -> a zipper without transformation.
        insert Transformation History -> pushes a transformation.
            (affecting state according to order!)

        state -> State at the current position in the stack
        prev -> travel down the stack
        next -> travel up the stack
        top -> travel upmost
        bottom -> suspend all transformations


    Transformation

    A transformation must satisfy the following interface:

        transform : Transformation -> State -> State
        invert : Transformation -> Transformation
            (where invert >> invert === id)
        isAbove : Transformation -> Transformation -> Bool
            (this way you can implement dependency graphs)
        isBelow : Transformation -> Transformation -> Bool

    State

        A state can be of any conceivable type.


    Since the stated purpose of History is to provide interchange
    of transformation stacks, transformations should of course be serializable.
-}





type History s =
    History { past: List ( Transformation s )
            , state: s
            , future: List ( Transformation s )
            }


singleton : State s -> History s
singleton s = History { past = [], state = s, future = [] }

insert : Transformation s -> History s -> History s
insert t ( History h ) =
    let here = History t::h.past ( transform t h.state ) h.future
        lower = prev >> insert t >> next
        higher = next >> insert t >> prev
    in case h of
        History [] state [] ->
            here
        History p::ast state [] ->
            if      Transformation.isBelow p t then lower else here
        History [] state f::uture ->
            if      Transformation.isAbove f t then higher else here
        History p::ast state f::uture ->
            if      Transformation.isBelow p t then lower
            else if Transformation.isAbove f t then higher
            else                                    here


prev : History s -> History s
prev ( History h ) =
    case h.past of
        [] ->
            identity
        p::ast ->
            History ast ( transform p h.state ) ( Transformation.invert p )::h.future

next : History s -> History s
next ( History h ) =
    case h.future of
        [] ->
            identity
        f::uture ->
            History ( Transformation.invert f )::h.past ( transform f h.state ) uture


state : History s -> s
state ( History h ) = h.state


isTop ( History h ) = h.future == []

top : History s -> History s
top = guard ( not<<isTop ) ( next>>top )


isBottom ( History h ) = h.bottom == []

bottom : History s -> History s
bottom = guard ( not<<isBottom ) ( prev>>bottom )








--------------- HELPERS ---------------

        
guard : ( a -> Bool ) -> ( a -> b ) -> a -> b
guard prediate change variable =
    if predicate variable then ( change variable ) else variable

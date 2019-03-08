module Transformation exposing
    ( Signature )
import State

{-
    TRANSFORMATION

    
    A transformation must satisfy the following interface:

        transform : Transformation -> State -> State
        invert : Transformation -> Transformation
            (where invert >> invert === id)
        isAbove : Transformation -> Transformation -> Bool
            (this way you can implement dependency graphs)
        isBelow : Transformation -> Transformation -> Bool


    We make this transformation module more specific in that we
    store Ordinal, Nominal and Contextual for single-dependency graphing.

    Additionally, a transformation has a serialized form/
-}



type Transformation s
    = Transformation { signature : Signature 
                     , function : State.Endofunction 
                     , inverse : State.Endofunction
                     }

{--
    Endofunctions advance the state.
    Each endofunction has exactly one inverse.
    
    An endofunction mutates one node. It either affects the node's data
    or its type ('line' of Ambilang).
    Data is referred to via Locus (a stack of concepts)
    while type is referred to via its signature of appending (a unique line).
    I.e. type references hinge on previous endofunctions while data
    references hinge on opaque 'current' state and may encompass zero or more lines.

    This has implications for later ('future') transformations:
    - setting data may influence infinitely many type nodes
    - setting type only ever influences one node
    - appending data adds a type node that may have an ambiguous definition ("multiprototype")
    - appending type adds a unique node (Zero)
    - choosing from a multiprototype is how data disambiguates type
    - choosing a filter is how type disambiguates data
    - moving data makes all previous data-transformations apply to a different locus,
      and these transformations may partially be inert in their new type.
    - moving type makes all previous type-transformations apply to a different context node,
      and this automatically includes the very definition of Locus, so this operation
      is isomorphic to a visual reordering in the combined tree of data and type!
    - Ack and Undo trace the synchronization history of the whole stack,
      so they are never affecting 'future' transformations.

    We get two sorts of ORPHANS:
    Locus orphan is a data manipulation on an erased locus.
    Signature orphan is a signature reference that doesn't yield.
    While the first is incurred by a type tree manipulation
    and may resolve once the tree is repaired,
    the latter should indeed not occur unless the network garbles
    transformations, and will be auto-erased on versioning.
--}


type alias Nominal = String

type alias Signature =
    { ordinal : Int -- locally generated increment. Unique only with Nominal.
    , nominal : Nominal -- remotely generated (random, unique) string.
    , contextual : Maybe Nominal -- reference to a Nominal if there is a unique context.
    }

-- signature of preceding transformation
precedencial : { t | signature : Signature } -> Signature
precedencial { transformation | signature } =
    case signature.ordinal of
        0 -> signature
        o -> { ordinal = o-1, nominal = signature.contextual, contextual = Nothing }


transform : Transformation s -> s -> s
transform t = ( t.signature.nominal, t.function ) >> State.permutation

invert : Transform s -> Transform s
invert ( Transformation signature function inverse ) = Transformation signature inverse function


isAbove { t | signature } { u | signature } =
    t.signature.ordinal > u.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( t.signature.ordinal > u.signature.ordinal )


isBelow { t | signature } { u | signature } =
    t.signature.ordinal < u.signature.ordinal
    || ( t.signature.ordinal == u.signature.ordinal ) && ( t.signature.ordinal < u.signature.ordinal )






-- SERIAL FORM

serialize : Transformation -> String
serialize ( Transformation t ) =
    

deserialize : String -> Transformation


-- helpers

padInt = String.fromInt >> pad

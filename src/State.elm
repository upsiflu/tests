module State
import Transformation exposing ( Signature, permutation )
import Stack exposing (Stack)

{-
    STATE

    This defines the structure and endofunctions of the state object.
    
    You can define a State for any kind of application.
    Given interfaces are
        
            / history \
            
        Transformation:
        Provide an Endofunction type with Inverses.
        Employ Transformation Signatures to call configuration atoms.


        UI:
        Provide a Children function and a root to populate a UI.
        Provide interactors and presentors for a Html interface.
        Expose global Undo.
        
            \ url and interactivity /

    We have a tree with two namespaces: Locus and Creation.
    Locus is a stack of Concepts,
    Creation is a Transformation.Signature.
        Creation is not "type".
-}


type State =
    State
    { -- cursors
      line : Transformation.Signature
    , locus : Locus
    , transformation : Transformation.Signature
    , form : Zipper Form
      -- caches
    , endofunctions : Dict Transformation.Signature Endofunction
    , flags : Dict Transformation.Signature Endofunction
    }

getChildLines : line -> Stack line
        

type Endofunction
    = -- what to delete and what to add.
      Modify String String
      -- adding a child node
    | Append
      -- removing an added Data/Type
    | Remove
      -- disambiguating a node (to Data/Type) 
    | Choose Transformation.Signature
      -- moving a node (to Data/Type) (where empty Locus = Start)
    | Move Transformation.Signature
      -- [flag-set] on node if that sig was recent trans 
    | Ack Transformation.Signature
      -- [flag-toggle]; UI translates global undo into recent me-undo
    | Undo Transformation.Signature
      -- affinity of the me-cursor
    | Target Vector

type Vector
    = T Transformation.Signature
    | L Locus


-- assemble a state permutation from a Nominal and one Endofuction
-- from this module's Endofunction type.
permutation : ( Transformation.Nominal, Endofunction ) -> ( State -> State )
permutation ( nominal, fu ) = case fu of
    Modify plus minus -> plusText plus >> minusText minus
    Append -> append
    Remove -> remove
    Choose t -> choose t
    Move t -> move t
    Ack t -> set ( Acked nominal ) t
    Undo t -> toggle ( Undone nominal ) t
    Target v -> walk v

walk : Vector -> ( State -> State )
walk v state = case v of
    T signature -> case ( state.endofunctions |> Dict.get signature ) of
        Append -> { state | form = state.form |> moveLine signature }
        _      -> { state | transformation = signature }
    L newLocus -> { state | locus = newLocus }

moveLine : Transition.Signature ( Zipper Form )
moveLine signature zipper = 
    if Transformation.isAbove ( current zipper ) { signature =  }

type Flag
    = Acked Transformation.Nominal
    | Undone Transformation.Nominal

set : Flag -> Transformation.signature -> ( State -> State )
set flag context state =
    case flag of
        Acked nom ->  { state | form = Form.ack  nom state.form }
        Undone nom -> { state | form = Form.undo nom state.form }

module Transformation

{-
    Transformation

    
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
    = Transformation { t : (s -> s) 
                     , i : (s -> s)
                     , ordinal : Int
                     , nominal : Int
                     , contextual : Int
                     }

serialize : Transformation -> String
serialize ( Transformation t ) =
    ...


deserialize : String -> Transformation

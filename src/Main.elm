import Browser
import Html exposing ( .. )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import App exposing ( .. )
import Stack exposing ( .. )  
import UI exposing ( .. ) 
import List exposing (map)   
          
main =
  Browser.sandbox { init = { app = App.alternativeApp }, update = update, view = UI.view ui >> .body }
      
type Msg
    = PutFocus ( Stack String )
    | Back   
         
update msg model =
  case msg of
    PutFocus path -> model   
    Back -> model


items = ( \state path ->
    { presentation = [ div [] [ App.getString path state.app |> text ] ]
    , interaction =              
        ring
            { interactivity = OK
            , presentation = [ div [class "focused"] [ App.getString path state.app |> text ] ] } []
    , role = Caption
    , childrenWrapper = ( \contents -> [ section [] contents ] )
    , childrenKeys = App.getFirstSteps path state.app
    } )
page =
    { prologue = \state -> [ h1 [] [ text "Tests: Decode and encode an App" ]
                           , p  [] ( App.encodeApp App.initialApp |> showInput )
                           , p  [] ( App.encodeApp App.alternativeApp |> showInput ) ] 
    , epilogue = [ text "////\\\\\\\\" ]          
    , meta = [ text "meta" ]
    , window = always []
    , focus = always []
    }  
navigate =
    { focus = PutFocus
    , back = Back
    , link = ( \x y -> "nixlink" )
    }
ui = UI.create items page navigate 
  
     
showApp : String -> List ( Html msg )
showApp = String.lines >> map ( showGroup >> p [ class "line" ] )
   
showGroup : String -> List ( Html msg )
showGroup = ( String.split " " ) >> map ( \t -> span [ class "word" ] [ text t ] )
   
showInput s = [ div [ contenteditable True, class "area"] [text s] ]
         
         

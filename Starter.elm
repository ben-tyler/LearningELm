module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random
import DrSprite exposing (..)

type Msg =
    Tick Float
    | KeyMsg Keyboard.Msg
      
type alias Model =
    { ticks: Int
     , fps: Float
     , pressedKeys: List Key
     }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]                 
       
view model =
    div []
        [ div [] [ text "hello world" ]
        , div [] [ text
                       <| "fps: " ++ String.fromFloat model.fps ]
        , div [] [ text
                       <| "ticks: " ++ String.fromInt model.ticks ]
                            
        ]
        
update msg model =
    case msg of
        Tick delta -> 
            ({ model | ticks = model.ticks + 1, fps = 1000 / delta }, Cmd.none)
        KeyMsg keyMsg ->
            ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys}, Cmd.none)


init _ =
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      }
      , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

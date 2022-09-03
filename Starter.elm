module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, text)
import Keyboard exposing (Key(..))
import DrSprite exposing (..)
import Time
import Browser.Events exposing (onAnimationFrame)

type Msg = 
    Tick Float
    | LockedTick Time.Posix
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
        , onAnimationFrame LockedTick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]                 
       
view : Model -> Html.Html msg
view model =
    div []
        [ div [] [ text "hello world" ]
        , div [] [ text
                       <| "fps: " ++ String.fromFloat model.fps ]
        , div [] [ text
                       <| "ticks: " ++ String.fromInt model.ticks ]
                            
        ]
        
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Tick _ -> 
            ( { model | ticks = model.ticks + 1 }, Cmd.none)
        KeyMsg keyMsg ->
            ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys}, Cmd.none)
        LockedTick _ -> 
            ( { model | ticks = model.ticks + 1 }
            , Cmd.none
            )

init : a -> (Model, Cmd msg)
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

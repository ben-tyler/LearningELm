module Snake exposing (..)

import Keyboard
import DrGame exposing (GameObject)
import DrGrid exposing (GameGridObject)
import DrSprite exposing (getSprite)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)

----------Move this
scale = 3

sv : DrSprite.Sprite -> Float -> (Float, Float) -> Html.Html msg
sv =  DrSprite.viewSprite "0x72_DungeonTilesetII_v1.4.png" scale

draw : GameObject -> Html.Html msg
draw gameobject =
    let 
        drawSprite =
            sv gameobject.sprite
               (toFloat gameobject.dir)
               (toFloat gameobject.x, toFloat gameobject.y)
     in
        drawSprite



-----------

type Msg = 
    Tick Float
    | KeyMsg Keyboard.Msg

type alias Model =
    { ticks: Int
    , fps: Float 
    , pressedKeys: List Keyboard.Key
    , gameGrid: List GameGridObject
    , snake: List GameGridObject
    , food: List GameGridObject
    }


init : a -> (Model, Cmd msg)
init _ =
    let
        snakeHead =
            getSprite
            |> DrGrid.gameGridObject (0, 0)
    in
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , gameGrid = getSprite 16 16 16 16 1
                   |> DrGrid.gameGrid 100
      , snake = [ snakeHead ]
      , food = []
      }
    , Cmd.none
    )

view : Model -> Html.Html msg
view model =
    div []
        ( [ text "hello world" ]
          ++ List.map (\i -> draw i.gameObject) model.gameGrid 
        )

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Tick delta ->
            (model, Cmd.none)
        KeyMsg keyMsg ->
            (model, Cmd.none)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
        

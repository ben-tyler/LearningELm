module Pokemon exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import DrGame exposing (GameObject)
import DrSprite
import Html exposing (div, text)
import Keyboard exposing (Key(..))
import Time



------------ SPRITE


scale =
    3


sv : DrSprite.Sprite -> Float -> ( Float, Float ) -> Html.Html msg
sv =
    DrSprite.viewSprite "0x72_DungeonTilesetII_v1.4.png" scale


draw : DrGame.GameObject -> Html.Html msg
draw gameobject =
    let
        drawSprite =
            sv gameobject.sprite
                (toFloat gameobject.dir)
                ( toFloat gameobject.x, toFloat gameobject.y )
    in
    drawSprite


drawGO : DrGame.GO -> Html.Html msg
drawGO go =
    let
        anim =
            go.spriteControl
    in
    case anim.current of
        DrSprite.Run ->
            sv anim.run
                (toFloat go.dir)
                ( toFloat go.x, toFloat go.y )

        DrSprite.Idle ->
            sv anim.idle
                (toFloat go.dir)
                ( toFloat go.x, toFloat go.y )


big_demon_idle_anim : DrSprite.Sprite
big_demon_idle_anim =
    DrSprite.getSprite 16 360 32 60 3


big_demon_run_anim : DrSprite.Sprite
big_demon_run_anim =
    DrSprite.getSprite 80 364 32 36 5



-----------


type Msg
    = Tick Float
    | LockedTick Time.Posix
    | KeyMsg Keyboard.Msg


type alias Model =
    { ticks : Int
    , fps : Float
    , pressedKeys : List Key
    , p1 : DrGame.GO
    , quit : Bool
    , daisy : DrGame.GO
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
        , div []
            [ text <|
                "fps: "
                    ++ String.fromFloat model.fps
            ]
        , div []
            [ text <|
                "ticks: "
                    ++ String.fromInt model.ticks
            ]
        , drawGO model.p1
        , drawGO model.daisy
        ]


gameLoop : Model -> Model
gameLoop model =
    let
        -- No issue with run sprite, I think it switches between them fine.
        -- just in one case run animation is not playing
        _ =
            Debug.log "m" (DrGame.moveOnKeyBoard model.pressedKeys)

        n =
            case DrGame.moveOnKeyBoard model.pressedKeys of
                ( 0, 0 ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Idle
                 --   model.p1
                    --DrGame.animateGO model.p1 model.ticks DrSprite.Idle
                   --     |> DrGame.moveGO 0 0

                ( x, y ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Run
                        |> DrGame.moveGO x y
    in
    { model
        | p1 = n
    }


checkQuit : Model -> Model
checkQuit model =
    if List.member Spacebar model.pressedKeys then
        { model | quit = True }

    else
        model


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        nModel =
            checkQuit model
    in
    if nModel.quit then
        ( nModel, Cmd.none )

    else
        case msg of
            Tick _ ->
                ( { nModel
                    | ticks = nModel.ticks + 1
                  }
                , Cmd.none
                )

            KeyMsg keyMsg ->
                ( { nModel | pressedKeys = Keyboard.update keyMsg nModel.pressedKeys }, Cmd.none )

            LockedTick _ ->
                ( gameLoop
                    { nModel
                        | ticks = nModel.ticks + 1
                    }
                , Cmd.none
                )


init : a -> ( Model, Cmd msg )
init _ =
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , p1 =
            { spriteControl =
                { current = DrSprite.Run
                , run = big_demon_run_anim
                , idle = big_demon_idle_anim
                }
            , x = 200
            , y = 200
            , dir = 1
            , gameGrid = Nothing
            , traveling = Nothing
            }
      , quit = False
      , daisy =
            { spriteControl =
                { current = DrSprite.Run
                , run = big_demon_run_anim
                , idle = big_demon_idle_anim
                }
            , x = 200
            , y = 200
            , dir = 1
            , gameGrid = Nothing
            , traveling = Nothing
            }
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

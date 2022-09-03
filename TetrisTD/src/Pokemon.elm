module Pokemon exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Debug exposing (toString)
import DrGame exposing (GO, GameObject, Intelligence)
import DrGrid exposing (GameGridObject, gameGrid, goMvGrid)
import DrSprite
import Html exposing (Html, div, text)
import Html.Attributes exposing (src, style, width)
import Keyboard exposing (Key(..))
import Task exposing (Task)
import Time



------------ SPRITE


scale : number
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


elf_f_idle_anim : DrSprite.Sprite
elf_f_idle_anim =
    DrSprite.getSprite 128 4 16 28 4


elf_f_run_anim : DrSprite.Sprite
elf_f_run_anim =
    DrSprite.getSprite 192 4 16 28 4


elf_f_hit_anim : DrSprite.Sprite
elf_f_hit_anim =
    DrSprite.getSprite 256 4 16 28 1


invis : DrSprite.Sprite
invis =
    DrSprite.getSprite 0 4 16 16 1



-----------


type Msg
    = Tick Float
    | LockedTick Time.Posix
    | KeyMsg Keyboard.Msg


type alias Model =
    { ticks : Int
    , start : Int
    , pressedKeys : List Key
    , p1 : GO
    , quit : Bool
    , daisy : GO
    , talkingToDaisy : Bool
    , map : List GO
    }


ladderSprite =
    DrSprite.getSprite 48 96 16 16 4


initMap : List DrGame.GO
initMap =
    DrGrid.fold2d
        { rows = 1, cols = 15 }
        (\( x, y ) result ->
            { spriteControl =
                { current = DrSprite.Idle
                , run = invis
                , idle = invis
                }
            , x = x * 16 * DrGame.scale
            , y = y * 16 * DrGame.scale + 500
            , dir = 1
            , gameGrid = Just ( x, y )
            , traveling = Nothing
            , intelligence = Nothing
            }
                :: result
        )
        []


init : a -> ( Model, Cmd msg )
init _ =
    ( { ticks = 1
      , start = 1
      , pressedKeys = []
      , p1 =
            { spriteControl =
                { current = DrSprite.Run
                , run = big_demon_run_anim
                , idle = big_demon_idle_anim
                }
            , x = 200
            , y = 500
            , dir = 1
            , gameGrid = Nothing
            , traveling = Nothing
            , intelligence =
                Just
                    { move = []
                    , speak =
                        [ "Hello daisy"
                        , "We have to feed the machine daisy..."
                        , "daiiiiiisssssyyyyyy"
                        , "let me out daisy I need to fee"
                        ]
                    }
            }
      , quit = False
      , daisy =
            { spriteControl =
                { current = DrSprite.Run
                , run = elf_f_idle_anim
                , idle = elf_f_idle_anim
                }
            , x = 0
            , y = 500
            , dir = 1
            , gameGrid = Just ( 10, 0 )
            , traveling = Just True
            , intelligence =
                Just
                    { move =
                        [ ( 9, 0 )
                        , ( 1, 0 )
                        , ( 5, 0 )
                        , ( 9, 0 )
                        , ( 9, 0 )
                        , ( 3, 0 )
                        ]
                    , speak = []
                    }
            }
      , map = initMap
      , talkingToDaisy = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , onAnimationFrame LockedTick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


viewTalkToDaisy : Model -> Html.Html msg
viewTalkToDaisy model =
    let
        talkText =
            case model.p1.intelligence of
                Nothing ->
                    ""

                Just int ->
                    case int.speak of
                        x :: xs ->
                            x

                        [] ->
                            ""
    in
    if model.talkingToDaisy then
        div
            [ style "background-image" "url('https://cdn.pixabay.com/photo/2013/07/13/13/20/bubble-160851_960_720.png')"
            , style "background-size" "200px"
            , style "position" "absolute"
            , style "left" (toString (model.p1.x - 50))
            , style "top" (toString (model.p1.y - 200))
            , style "width" "200"
            , style "height" "140"
            , style "transform" "scaleX(1)"
            ]
            [ div
                [ style "position" "absolute"
                , style "left" "50px"
                , style "top" "50px"
                ]
                [ text <|
                    talkText
                ]
            ]

    else
        div [] []


view : Model -> Html.Html msg
view model =
    div []
        [ div [] [ text "hello world" ]
        , div []
            [ text <|
                "ticks: "
                    ++ String.fromInt model.ticks
            ]
        , Html.img
            [ src "https://cdn.dribbble.com/users/119313/screenshots/1681630/media/8e5179ea4d3045dfe6578a1642ffcb33.gif"
            , width 700
            ]
            []
        , div [] (List.map (\i -> drawGO i) model.map)
        , drawGO model.daisy
        , drawGO model.p1
        , viewTalkToDaisy model
        ]


gameLoop : Model -> Model
gameLoop model =
    let
        -- No issue with run sprite, I think it switches between them fine.
        -- just in one case run animation is not playing
        -- _ =
        --     Debug.log "m" controllkDiasy
        controllPlayer =
            case DrGame.moveOnKeyBoard model.pressedKeys of
                ( 0, 0 ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Idle

                ( x, _ ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Run
                        |> DrGame.moveGO x 0

        controllkDiasy : GO
        controllkDiasy =
            DrGame.animateGO model.daisy model.ticks DrSprite.Idle
                |> goMvGrid model.map

        controllTalk =
            DrGame.itCollides (DrGame.goBoundingBox model.p1 8) (DrGame.goBoundingBox model.daisy 8)

        p1 =
            model.p1

        controllSpeakIntelligence p1go =
            if model.talkingToDaisy && controllTalk == False then
                { p1go
                    | intelligence =
                        case p1go.intelligence of
                            Just p1intel ->
                                case p1intel.speak of
                                    _ :: xs ->
                                        Just
                                            { p1intel
                                                | speak = xs
                                            }

                                    [] ->
                                        Just p1intel

                            _ ->
                                Nothing
                }

            else
                p1go

        controllMoveIntelligence : GO -> GO
        controllMoveIntelligence p1go =
            let
                ( newIntelligence, newGameGrid ) =
                    case p1go.intelligence of
                        Just p1intel ->
                            case p1intel.move of
                                x :: xs ->
                                    ( Just
                                        { p1intel
                                            | move = xs
                                        }
                                    , Just x
                                    )

                                [] ->
                                    ( Just p1intel, Nothing )

                        Nothing ->
                            ( Nothing, Nothing )
            in
            if model.talkingToDaisy == False && controllTalk == True then
                { p1go
                    | intelligence = newIntelligence
                    , gameGrid = newGameGrid
                }

            else
                p1go
    in
    { model
        | p1 =
            controllPlayer
                |> controllSpeakIntelligence
        , daisy =
            controllkDiasy
                |> controllMoveIntelligence
        , talkingToDaisy = controllTalk
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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

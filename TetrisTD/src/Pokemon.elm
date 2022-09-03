module Pokemon exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Debug exposing (toString)
import DrGame exposing (Action(..), GO, GameObject, Intelligence)
import DrGrid exposing (GameGridObject, gameGrid, goMvGrid)
import DrSprite
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (height, src, style, width)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
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
    DrSprite.getSprite 128 4 16 28 1


elf_f_run_anim : DrSprite.Sprite
elf_f_run_anim =
    DrSprite.getSprite 128 4 16 28 4


elf_f_hit_anim : DrSprite.Sprite
elf_f_hit_anim =
    DrSprite.getSprite 256 4 16 28 1


invis : DrSprite.Sprite
invis =
    DrSprite.getSprite 0 4 16 16 1


chest_empty_open_anim =
    DrSprite.getSprite 304 288 16 16 3



-----------


type Msg
    = Tick Float
    | LockedTick Time.Posix
    | KeyMsg Keyboard.Msg
    | MoveRight
    | MoveLeft
    | MouseUp


type alias Model =
    { ticks : Int
    , start : Int
    , pressedKeys : List Key
    , p1 : GO
    , quit : Bool
    , daisy : GO
    , box : GO
    , daisyCarriesBox: Bool
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
            , y = y * 16 * DrGame.scale + 530
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
            , y = 530
            , dir = 1
            , gameGrid = Nothing
            , traveling = Nothing
            , intelligence =
                Just
                    { move = []
                    , speak =
                        [ "Good morning daisy"
                        , "Its time to get to work, we have quartily growth to meet"
                        , "daiiiiiisssssyyyyyy"
                        , "we need to achieve sustrainability via our qurtily growth"
                        , "through hard work, you can live up to your potential"
                        , "with climbing prodactivity our sustanable quartaly growth"
                        , "meeeeting the potential of share holders"
                        , "excellent box managements daiissssy"
                        , "synagise box management skils"
                        , "intergrate box management vertiacally to achieve sustainable quartily grown"
                        , ""
                        , "you have a bright future at sustainable box quartily growth"
                        , ""
                        ," put box for more prodactivty"
                        , "amazing promotion for middle management quarityly review next yeaR!"
                        ]
                    }
            }
      , quit = False
      , daisy =
            { spriteControl =
                { current = DrSprite.Run
                , run = elf_f_run_anim
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
                        [ ( 9, 0, NoAction )
                        , ( 1, 0, NoAction )
                        , ( 5, 0, NoAction )
                        , ( 0, 0, NoAction )
                        , ( 9, 0, NoAction )
                        , ( 3, 0, PickUpBox )
                        , ( 9, 0, NoAction )
                        , ( 3, 0, NoAction )
                        , ( 9, 0, PutDownBox )
                        , ( 5, 0, NoAction )
                        , ( 0, 0, NoAction )
                        , ( 9, 0, NoAction )
                        , ( 3, 0, PickUpBox )
                        , ( 9, 0, NoAction )
                        , ( 3, 0, NoAction )
                        , ( 9, 0, PutDownBox )
                        , ( 3, 0, NoAction )
                        ]
                    , speak = []
                    }
            }
      , box =
            { spriteControl =
                { current = DrSprite.Run
                , run = chest_empty_open_anim
                , idle = chest_empty_open_anim
                }
            , x = -100
            , y = -100
            , dir = 1
            , gameGrid = Nothing
            , traveling = Nothing
            , intelligence = Nothing
            }
      , daisyCarriesBox = False
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
    if model.talkingToDaisy && talkText /= "" then
        div
            [ style "background-image" "url('https://cdn.pixabay.com/photo/2013/07/13/13/20/bubble-160851_960_720.png')"
            , style "background-size" "200px"
            , style "position" "absolute"
            , style "left" <| String.fromInt (model.p1.x - 50)  ++ "px"
            , style "top" <| String.fromInt (model.p1.y - 200)  ++ "px"
            , style "width" "200px"
            , style "height" "140px"
            , style "transform" "scaleX(1)"
            ]
            [ div
                [ 
                    style "position" "absolute"
                 , style "left" "10px"
                , style "top" "30px"
                ]
                [ text <|
                    talkText
                ]
            ]

    else
        div [] []


view model =
    div []
        [ div []
            [ text "dad controlls"
            , button
                [ onMouseDown MoveLeft
                , onMouseUp MouseUp
                , style "display" "inline-block"
                , style "margin" "10px 10px"
                , style "padding" "15px 32px"
                ]
                [ text "Move Left" ]
            , button
                [ onMouseDown MoveRight
                , onMouseUp MouseUp
                , style "display" "inline-block"
                , style "margin" "10px 10px"
                , style "padding" "15px 32px"
                ]
                [ text "Move Right" ]
            ]
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

        --  , div [] (List.map (\i -> drawGO i) model.map)
        , drawGO model.box
        , drawGO model.daisy
        , drawGO model.p1
        , viewTalkToDaisy model
        ]


gameLoop : Model -> Model
gameLoop model =
    let
        controllPlayer : GO
        controllPlayer =
            case DrGame.moveOnKeyBoard model.pressedKeys of
                ( 0, 0 ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Idle

                ( x, _ ) ->
                    DrGame.animateGO model.p1 model.ticks DrSprite.Run
                        |> DrGame.moveGO x 0

        controllkDiasy : GO
        controllkDiasy = 
            case model.daisy.traveling of
                Just True ->  
                    DrGame.animateGO model.daisy model.ticks DrSprite.Run
                        |> goMvGrid model.map
                _ -> 
                    DrGame.animateGO model.daisy model.ticks DrSprite.Idle
                        |> goMvGrid model.map

        controllTalk : Bool
        controllTalk =
            DrGame.itCollides (DrGame.goBoundingBox model.p1 6) (DrGame.goBoundingBox model.daisy 6)
                |> (\collision ->
                        case DrGame.moveOnKeyBoard model.pressedKeys of
                            ( 0, 0 ) ->
                                if model.talkingToDaisy then
                                    True

                                else
                                    collision

                            ( x, _ ) ->
                                False
                   )

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

        controllMoveIntelligence : GO -> (GO, Bool)
        controllMoveIntelligence p1go =
            let
                ( newIntelligence, newGameGrid, carrybox ) =
                    case p1go.intelligence of
                        Just p1intel ->
                            case p1intel.move of
                                x :: xs ->
                                    ( Just
                                        { p1intel
                                            | move = xs
                                        }
                                    , Just x
                                    , case x of
                                        (_, _, PickUpBox) -> 
                                            True
                                        (_, _, NoAction) -> 
                                            model.daisyCarriesBox
                                        (_, _, PutDownBox) -> 
                                            False

                                     )

                                [] ->
                                    ( Just p1intel, Nothing, model.daisyCarriesBox )

                        Nothing ->
                            ( Nothing, Nothing, model.daisyCarriesBox)

                res =
                    case newGameGrid of
                        Just ( ngx, ngy, _ ) ->
                            Just ( ngx, ngy )

                        Nothing ->
                            Nothing

            in
            if model.talkingToDaisy == False && controllTalk == True then
                ({ p1go
                    | intelligence = newIntelligence
                    , gameGrid = res
                    , traveling = Just True
                }, carrybox)

            else
                (p1go, carrybox)

        (dayDay, cb) = 
          controllkDiasy
                |> controllMoveIntelligence

        box = 
            let
                mb = model.box
            in
            
            if cb then 
                {mb
                | x = dayDay.x
                , y = dayDay.y - 20}
            else 
                if mb.x  > 0 && mb.x < 700 then 
                    {mb
                    | x = mb.x + 1
                    , y = dayDay.y - 70}
                else 
                    {mb
                    | x = -100
                    , y = -100}
    in
    { model
        | p1 =
            controllPlayer
                |> controllSpeakIntelligence
        , daisy = dayDay
        , talkingToDaisy = controllTalk
        , daisyCarriesBox = cb
        , box = box
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

            MoveLeft ->
                ( { nModel | pressedKeys = [ ArrowLeft ] }, Cmd.none )

            MoveRight ->
                ( { nModel | pressedKeys = [ ArrowRight ] }, Cmd.none )

            MouseUp ->
                ( { nModel | pressedKeys = [] }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

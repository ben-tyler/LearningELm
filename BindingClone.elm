module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random


type alias Sprite =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , frames : Float
    , currentFrame : Float
    }


getSprite x y w h f =
    { x = x
    , y = y
    , w = w
    , h = h
    , frames = f
    , currentFrame = 1
    }


type alias Cords =
    ( Float, Float )


type alias Player =
    { location : Cords
    , direction : Cords
    , sprite : Sprite
    , dead : Bool
    }


type alias Bullet =
    { location : Cords
    , direction : Cords
    , distance : Float
    , sprite : Sprite
    }


type alias Enemy =
    { location : Cords
    , sprite : Sprite
    }


type alias Model =
    { count : Float
    , fps : Float
    , pressedKeys : List Keyboard.Key
    , player : Player
    , bullets : List Bullet
    , enemies : List Enemy
    }


floor_1 =
    getSprite 16 64 16 16 1


floor_2 =
    getSprite 32 64 16 16 1


floor_3 =
    getSprite 48 64 16 16 1


floor_4 =
    getSprite 16 80 16 16 1


floor_5 =
    getSprite 32 80 16 16 1


skull =
    getSprite 288 320 16 16 1


elf_f_idle_anim =
    getSprite 128 4 16 28 4


elf_f_run_anim =
    getSprite 192 4 16 28 4


elf_f_hit_anim =
    getSprite 256 4 16 28 1


elf_m_idle_anim =
    getSprite 128 36 16 28 4


elf_m_run_anim =
    getSprite 192 36 16 28 4


elf_m_hit_anim =
    getSprite 256 36 16 28 1


knight_f_idle_anim =
    getSprite 128 68 16 28 4


knight_f_run_anim =
    getSprite 192 68 16 28 4


knight_f_hit_anim =
    getSprite 256 68 16 28 1


knight_m_idle_anim =
    getSprite 128 100 16 28 4


knight_m_run_anim =
    getSprite 192 100 16 28 4


knight_m_hit_anim =
    getSprite 256 100 16 28 1


wizzard_f_idle_anim =
    getSprite 128 132 16 28 4


wizzard_f_run_anim =
    getSprite 192 132 16 28 4


wizzard_f_hit_anim =
    getSprite 256 132 16 28 1


wizzard_m_idle_anim =
    getSprite 128 164 16 28 4


wizzard_m_run_anim =
    getSprite 192 164 16 28 4


wizzard_m_hit_anim =
    getSprite 256 164 16 28 1


lizard_f_idle_anim =
    getSprite 128 196 16 28 4


lizard_f_run_anim =
    getSprite 192 196 16 28 4


lizard_f_hit_anim =
    getSprite 256 196 16 28 1


lizard_m_idle_anim =
    getSprite 128 228 16 28 4


lizard_m_run_anim =
    getSprite 192 228 16 28 4


lizard_m_hit_anim =
    getSprite 256 228 16 28 1


scaleFactor =
    3


init _ =
    ( { count = 1
      , fps = 0
      , pressedKeys = []
      , player =
            { location = ( 200, 200 )
            , direction = ( 1, 0 )
            , sprite =
                --getSprite 368 16 16 16 4
                wizzard_f_run_anim
            , dead = False
            }
      , bullets = []
      , enemies =
            [ { location = ( 100, 100 ), sprite = lizard_m_idle_anim }
            , { location = ( 300, 300 ), sprite = knight_m_idle_anim }
            ]
      }
    , Cmd.none
    )


type Msg
    = Frame Float
    | KeyMsg Keyboard.Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Frame
        , Sub.map KeyMsg Keyboard.subscriptions
        ]



------------ Update --------


isShooting : List Keyboard.Key -> Bool
isShooting pressedKeys =
    List.member Spacebar pressedKeys


moveOnKeyBoard : Cords -> List Keyboard.Key -> Cords
moveOnKeyBoard cords pressedKeys =
    let
        arrows =
            Keyboard.Arrows.arrows pressedKeys

        ( x, y ) =
            cords
    in
    ( x + toFloat arrows.x, y - toFloat arrows.y )


setDirectionOnKeyBoard : List Keyboard.Key -> Cords
setDirectionOnKeyBoard pressedKeys =
    let
        arrows =
            Keyboard.Arrows.arrows pressedKeys
    in
    ( toFloat arrows.x, toFloat -arrows.y )


moveBullet bullet =
    let
        ( dx, dy ) =
            bullet.direction

        ( bx, by ) =
            bullet.location
    in
    { bullet
        | distance = bullet.distance + 1
        , location = ( bx + dx, by + dy )
    }


handleBullets model =
    let
        newBullets =
            case isShooting model.pressedKeys of
                True ->
                    { location = model.player.location
                    , direction = model.player.direction
                    , distance = 0
                    , sprite = skull
                    }
                        :: model.bullets

                False ->
                    model.bullets
    in
    List.map moveBullet newBullets
        |> List.foldl
            (\n accum ->
                if n.distance > 200 then
                    accum

                else
                    n :: accum
            )
            []


setLastDirection oldDirection pressedKeys =
    let
        dir =
            setDirectionOnKeyBoard pressedKeys
    in
    if dir == ( 0, 0 ) then
        oldDirection

    else
        dir


type alias BoundingBox =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


getBoundingBox ( x, y ) width height =
    { x = x
    , y = y
    , w = width * scaleFactor
    , h = height * scaleFactor
    }


itCollides : BoundingBox -> BoundingBox -> Bool
itCollides b1 b2 =
    b1.x
        < b2.x
        + b2.w
        && b1.x
        + b1.w
        > b2.x
        && b1.y
        < b2.y
        + b2.h
        && b1.h
        + b1.y
        > b2.y


handleSprite : Sprite -> Float -> Sprite
handleSprite sprite count =
    let
        --_ =
        --    Debug.log "called" (String.fromInt sprite.currentFrame)
        withNextFrame =
            if sprite.currentFrame == sprite.frames then
                { sprite
                    | currentFrame = 1
                }

            else
                { sprite
                    | currentFrame = sprite.currentFrame + 1
                }
    in
    if modBy 25 (round count) == 0 then
        withNextFrame

    else
        sprite


checkBullets : Enemy -> List Bullet -> Bool
checkBullets en bullets =
    List.foldl
        (\b acc ->
            let
                b1 =
                    getBoundingBox b.location b.sprite.w b.sprite.h

                b2 =
                    getBoundingBox en.location en.sprite.w en.sprite.h
            in
            if acc then
                True

            else
                itCollides b1 b2
        )
        False
        bullets


type MyMsg
    = Rand Int


genRoll =
    Random.generate Rand (Random.int -1 1)


handleEnemies : Model -> List Enemy
handleEnemies model =
    let
        removeDead =
            List.foldl
                (\e acc ->
                    if checkBullets e model.bullets then
                        acc

                    else
                        e :: acc
                )
                []
                model.enemies

        updateSprite =
            List.map
                (\en ->
                    { en
                        | sprite = handleSprite en.sprite model.count
                    }
                )
                removeDead

        doWalk =
            List.map
                (\en ->
                    let
                        ( x, y ) =
                            en.location

                        ( rdx, rdy ) =
                            if
                                x
                                    > 600
                                    || y
                                    > 600
                                    || x
                                    < 0
                                    || y
                                    < 0
                            then
                                ( 0, 0 )

                            else
                                model.player.direction
                    in
                    { en
                        | location = ( x + rdx, y + rdy )
                    }
                )
                updateSprite

        newEnemy =
            if modBy 300 (round model.count) == 0 then
                [ { location = ( 300, 300 ), sprite = knight_m_idle_anim } ]

            else
                []
    in
    doWalk ++ newEnemy


handlePlayer : Model -> Player
handlePlayer model =
    let
        checkIfDead =
            List.foldl
                (\en a ->
                    let
                        b1 =
                            getBoundingBox model.player.location
                                model.player.sprite.w
                                model.player.sprite.h

                        b2 =
                            getBoundingBox en.location
                                en.sprite.w
                                en.sprite.h
                    in
                    if a then
                        a

                    else
                        itCollides b1 b2
                )
                False
                model.enemies
    in
    { location =
        moveOnKeyBoard model.player.location model.pressedKeys
    , direction =
        setLastDirection model.player.direction model.pressedKeys
    , sprite =
        if model.player.dead then
            wizzard_m_hit_anim

        else
            handleSprite model.player.sprite model.count
    , dead =
        if model.player.dead then
            True

        else
            checkIfDead
    }


tick : Model -> Float -> Model
tick model delta =
    { model
        | count = model.count + 1
        , fps = 1000 / delta
        , player = handlePlayer model
        , bullets = handleBullets model
        , enemies = handleEnemies model
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            ( { model
                | pressedKeys =
                    Keyboard.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )

        Frame delta ->
            ( tick model delta
            , Cmd.none
            )



---------- VIEW ---


testImage =
    "https://i0.wp.com/i.imgur.com/s4ZdIyt.gif?resize=325,200"


tilesheet =
    "src/0x72_DungeonTilesetII_v1.4.png"


cssCircle =
    [ style "width" <|
        String.fromFloat 20
    , style "height" <|
        String.fromFloat 20
    , style "border-radius" "50%"
    , style "background-color" "aqua"
    ]


cssPosition ( x, y ) =
    [ style "position" "absolute"
    , style "left" <| String.fromFloat x
    , style "top" <| String.fromFloat y
    ]


drawbullets bullets =
    List.map
        (\i ->
            div (viewSprite i.sprite i.direction ++ cssPosition i.location) []
        )
        bullets


setBackgroundPosition : Sprite -> String
setBackgroundPosition sprite =
    let
        yloc =
            sprite.y

        xloc =
            sprite.x + sprite.currentFrame * sprite.w
    in
    String.fromFloat (xloc * -1)
        ++ "px "
        ++ String.fromFloat (yloc * -1)
        ++ "px"


viewSprite sprite ( xdir, _ ) =
    let
        spriteDirection =
            if xdir == 0 then
                String.fromFloat 1

            else
                String.fromFloat xdir
    in
    [ style "background" "url(src/0x72_DungeonTilesetII_v1.4.png) no-repeat"
    , style "width" <| String.fromFloat sprite.w
    , style "height" <| String.fromFloat sprite.h
    , style "background-position" <| setBackgroundPosition sprite
    , style "transform" ("scale(" ++ String.fromInt scaleFactor ++ ") scaleX(" ++ spriteDirection ++ ")")
    ]


drawMap =
    let
        tiles =
            List.range 1 30
                |> List.map
                    (\i ->
                        div (viewSprite floor_1 ( toFloat i, toFloat i )) []
                    )
    in
    tiles


viewData model =
    [ div [] [ text "View Data" ]
    , div [] [ text ("fps : " ++ String.fromFloat model.fps) ]
    , div [] [ text ("clock : " ++ String.fromFloat model.count) ]
    ]


viewPlayer model =
    [ let
        playerSprite =
            viewSprite model.player.sprite model.player.direction

        playerPosition =
            cssPosition model.player.location
      in
      div (playerSprite ++ playerPosition) []
    ]


viewEnemies model =
    List.map
        (\en ->
            div (viewSprite en.sprite model.player.direction ++ cssPosition en.location) []
        )
        model.enemies


view model =
    div []
        (drawMap
            ++ viewPlayer model
            ++ drawbullets model.bullets
            ++ viewEnemies model
            ++ viewData model
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

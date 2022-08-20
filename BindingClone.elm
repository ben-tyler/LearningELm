module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows


type alias Sprite =
    { x : Int, y : Int, w : Int, h : Int, frames : Int, currentFrame : Int }


getSprite x y w h f =
    { x = x, y = y, w = w, h = h, frames = f, currentFrame = 1 }


type alias Cords =
    ( Float, Float )


type alias Player =
    { location : Cords, direction : Cords, sprite : Sprite }


type alias Bullet =
    { location : Cords, direction : Cords, distance : Float }


type alias Enemy =
    { location : Cords }


type alias Model =
    { count : Float
    , pressedKeys : List Keyboard.Key
    , player : Player
    , bullets : List Bullet
    , enemies : List Enemy
    }


init _ =
    ( { count = 1
      , pressedKeys = []
      , player =
            { location = ( 200, 200 )
            , direction = ( 1, 0 )
            , sprite =
                --getSprite 368 16 16 16 4
                getSprite 368 204 16 20 4
            }
      , bullets = []
      , enemies = []
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


tick : Model -> Float -> Model
tick model delta =
    { model
        | count = model.count + 1
        , player =
            { location = moveOnKeyBoard model.player.location model.pressedKeys
            , direction = setLastDirection model.player.direction model.pressedKeys
            , sprite = handleSprite model.player.sprite model.count
            }
        , bullets = handleBullets model
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


cssPosition x y =
    [ style "position" "absolute"
    , style "left" <| String.fromFloat x
    , style "top" <| String.fromFloat y
    ]


drawbullets bullets =
    List.map
        (\i ->
            let
                ( bx, by ) =
                    i.location
            in
            div (cssCircle ++ cssPosition bx by) []
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
    String.fromInt (xloc * -1)
        ++ "px "
        ++ String.fromInt (yloc * -1)
        ++ "px"


view model =
    let
        ( xdir, _ ) =
            model.player.direction

        spriteDirection =
            if xdir == 0 then
                String.fromFloat 1

            else
                String.fromFloat xdir

        ( px, py ) =
            model.player.location

        sprite =
            getSprite 368 16 16 16 4
    in
    div []
        ([ div
            ([ style "background" "url(src/0x72_DungeonTilesetII_v1.4.png) no-repeat"
             , style "width" <| String.fromInt model.player.sprite.w
             , style "height" <| String.fromInt model.player.sprite.h
             , style "background-position" <| setBackgroundPosition model.player.sprite
             , style "transform" ("scale(4) scaleX(" ++ spriteDirection ++ ")")
             ]
                ++ cssPosition px py
            )
            []

         --, img
         --   [ src tilesheet
         --   , style "clip-path" "inset(20px 20px)"
         --   ]
         --   []
         , div [] [ text ("fps: " ++ String.fromFloat model.count) ]

         --, img ([ src testImage, width 100, height 100 ] ++ cssPosition px py) []
         ]
            ++ drawbullets model.bullets
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

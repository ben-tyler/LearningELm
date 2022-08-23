module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random
import DrSprite

fold2d :
    { rows : Int, cols : Int }
    -> (( Int, Int ) -> result -> result)
    -> result
    -> result
fold2d { rows, cols } fn initial =
    let
        iter x y res =
            if y >= rows then
                res

            else if x >= cols then
                iter 0 (y + 1) res

            else
                iter (x + 1) y (fn ( x, y ) res)
    in
    iter 0 0 initial
 
type alias GameObject =
    { sprite: DrSprite.Sprite
    , x: Int
    , y: Int
    }

type alias GameState =
    { gameGrid: List (Int, Int)
    , hand: (Int, Int)
    }

type Msg =
    Tick Float
    | KeyMsg Keyboard.Msg
      
type alias Model =
    { ticks: Int
     , fps: Float
     , pressedKeys: List Key
     , gameObjects: List GameObject
     , gameState: GameState
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
        
scale : number
scale = 3
sv : DrSprite.Sprite -> (Float, Float) -> (Float, Float) -> Html.Html msg
sv =  DrSprite.viewSprite  --"https://cdn.pixabay.com/photo/2015/04/23/22/00/tree-736885_960_720.jpg" scale
    "0x72_DungeonTilesetII_v1.4.png" scale

draw sprite x y =
    let 
        drawSprite =
            sv sprite
               (0, 0)
               (toFloat x, toFloat y)
     in
        drawSprite

drawTile sprite x y =
    let
        offset = 100
                
        posx =
            toFloat x * 16 * scale

        posy =
            toFloat y * 16 * scale
                   
        drawSprite =
            sv sprite
               (0, 0)
               (posx + offset, posy + offset)
     in                   
     drawSprite

drawMap =
    let
        tile =
            DrSprite.getSprite 16 64 16 16 1
    in
    fold2d
        {rows = 9, cols = 9}
        (\ (x, y) result -> drawTile tile x y :: result)
        []

drawGameObjects model =
    List.map ( \ n -> draw n.sprite n.x n.y ) model.gameObjects
    
view model =
    div []
        ([ div [] [ text "hello world" ]
        , div [] [ text
                      <| "fps: " ++ String.fromFloat model.fps ]
        , div [] [ text
                       <| "ticks: " ++ String.fromInt model.ticks ]
        ]
             ++ drawMap
             ++ drawGameObjects model
        )


---- Game -----
gameFun : GameState -> (GameState, List GameObject) 
gameFun gamestate = 
    let
        (dx, dy) = gamestate.hand
        nhand = (dx + 1, dy)
    in
    
    ( {gamestate | hand = nhand}
    , [
        { sprite =  lizzardSprite
        , x = dx
        , y = dy
        }
    ] 
    )



        
-- initializer         
lizzardSprite = 
    DrSprite.getSprite 192 228 16 28 4

ladderSprite =
    DrSprite.getSprite 48 96 16 16 4


floorSpikes =
    DrSprite.getSprite 16 176 16 16 4

swordSprite  =
    DrSprite.getSprite 316 176 16 16 
        
generateGameGrid =
    fold2d { rows = 9, cols = 9 }
        (\ i r -> i :: r )
        []
        
init _ =
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , gameObjects =
            [ { sprite = floorSpikes
              , x = 100
              , y = 100
              }
--            , { sprite = lizzardSprite 
--              , x = 150
--              , y = 100
--              }
            ]
      , gameState =
            { gameGrid = generateGameGrid
            , hand = (1,1)
            }
        }
      , Cmd.none
    )



    --------- UPDATE --------
    --------- UPDATE --------

animateSprite sprite =
    let
        nextFrame =
            if sprite.currentFrame == sprite.frames then
                1
            else
                sprite.currentFrame + 1
    in
    { sprite | currentFrame = nextFrame }

animateGameObjects gameObjects =
    List.map (\g ->
             { g| sprite = animateSprite g.sprite } )
        gameObjects

animator : Model -> Model
animator model =
    if modBy 25 model.ticks /= 0 then
        model
    else
        { model | gameObjects = animateGameObjects model.gameObjects } 


tickGame : Model -> Model
tickGame model =
    let
        (nextGameState, nextGameObjects) 
            = gameFun model.gameState
    in
    
    { model | gameState = nextGameState
    , gameObjects = nextGameObjects
    }
    
ticker delta model =
            animator model
            --|> tickGame
            |> (\ nextModel ->
                    ({ nextModel | ticks = model.ticks + 1, fps = 1000 / delta }
                    , Cmd.none ) )

    
update msg model =
    case msg of
        Tick delta ->
            ticker delta model
        KeyMsg keyMsg ->
            ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys}, Cmd.none)



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

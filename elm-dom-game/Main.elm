module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random
import DrSprite
import DrSprite exposing (Sprite)

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
    , dir: Int
    }

type alias GameState =
    { gameGrid: List (Int, Int)
    , hand: GameObject
    , spike: GameObject
    , dead: Bool
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
sv : DrSprite.Sprite -> Float -> (Float, Float) -> Html.Html msg
sv =  DrSprite.viewSprite  --"https://cdn.pixabay.com/photo/2015/04/23/22/00/tree-736885_960_720.jpg" scale
    "0x72_DungeonTilesetII_v1.4.png" scale

draw : GameObject -> Html.Html msg
draw gameobject =
    let 
        drawSprite =
            sv gameobject.sprite
               (toFloat gameobject.dir)
               (toFloat gameobject.x, toFloat gameobject.y)
     in
        drawSprite

drawTile : Sprite -> Int -> Int -> Html.Html msg
drawTile sprite x y =
    let
        offset = 100
                
        posx =
            toFloat x * 16 * scale

        posy =
            toFloat y * 16 * scale
                   
        drawSprite =
            sv sprite
               1
               (posx + offset, posy + offset)
     in                   
     drawSprite

drawMap : List (Html.Html msg)
drawMap =
    let
        tile =
            DrSprite.getSprite 16 64 16 16 1
    in
    fold2d
        {rows = 9, cols = 9}
        (\ (x, y) result -> drawTile tile x y :: result)
        []

drawGameObjects : Model -> List (Html.Html msg)
drawGameObjects model =
    List.map ( \ n -> draw n ) model.gameObjects
    
view : Model -> Html.Html msg
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
             ++ if model.gameState.dead then 
                    [div [][text "YOU ARE DEAD"]]
                else 
                    [div [][]]
        )


---- Game -----
gameFun : GameState -> (GameState, List GameObject) 
gameFun gamestate = 
    let
        hand = gamestate.hand
        nextHand = { hand | x = hand.x + 1 }
    in
    
    ( {gamestate | hand = nextHand}
    , [
   --     nextHand
      ] 
    )



        
-- initializer         
lizzardSprite : Sprite
lizzardSprite = 
    DrSprite.getSprite 192 228 16 28 4

ladderSprite : Sprite
ladderSprite =
    DrSprite.getSprite 48 96 16 16 4


floorSpikes : Sprite
floorSpikes =
    DrSprite.getSprite 16 176 16 16 4

swordSprite : Float -> Sprite
swordSprite  =
    DrSprite.getSprite 316 176 16 16 
        
generateGameGrid : List (Int, Int)
generateGameGrid =
    fold2d { rows = 9, cols = 9 }
        (\ i r -> i :: r )
        []
        
init : a -> (Model, Cmd msg)
init _ =
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , gameObjects =
            [ 
            ]
      , gameState =
            { gameGrid = generateGameGrid
            , hand = 
                { sprite = lizzardSprite 
                , x = 0
                , y = 0
                , dir = 1
                }
            , spike = { sprite = floorSpikes
              , x = 100
              , y = 100
              , dir = 1
              }
              , dead = False
            }
        }
      , Cmd.none
    )



    --------- UPDATE --------
    --------- UPDATE --------

animateSprite : Sprite -> Sprite
animateSprite sprite =
    let
        nextFrame =
            if sprite.currentFrame == sprite.frames then
                1
            else
                sprite.currentFrame + 1
    in
    { sprite | currentFrame = nextFrame }

animateGameObject : GameObject -> Int -> GameObject
animateGameObject gameObject ticks =
    if modBy 25 ticks /= 0 then
        gameObject
    else
        { gameObject| sprite = animateSprite gameObject.sprite } 


moveOnKeyBoard : List Keyboard.Key -> (Int, Int)
moveOnKeyBoard pressedKeys =
    let
        arrows =
            Keyboard.Arrows.arrows pressedKeys
    in
    ( arrows.x, arrows.y * -1 )


type alias BoundingBox =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


getBoundingBox : GameObject -> BoundingBox
getBoundingBox gameObject =
    { x = gameObject.x
    , y = gameObject.y
    , w = round (gameObject.sprite.w * scale)
    , h = round (gameObject.sprite.h * scale)
    }

itCollides : BoundingBox -> BoundingBox -> Bool
itCollides b1 b2 =
    b1.x < b2.x + b2.w && b1.x + b1.w > b2.x && b1.y < b2.y + b2.h && b1.h + b1.y > b2.y

moveGameObject : Int -> Int -> GameObject -> GameObject
moveGameObject  keyx keyy gameObject = 
    { gameObject | x = gameObject.x + keyx
                 , y = gameObject.y + keyy
                 , dir = keyx }

ticker : Float -> Model -> ( Model, Cmd msg)
ticker delta model =
    let 
      (keyx, keyy ) = moveOnKeyBoard model.pressedKeys
      hs = animateGameObject model.gameState.hand model.ticks
        |> moveGameObject keyx keyy
      ss = animateGameObject model.gameState.spike model.ticks
     
      gs = model.gameState
      nextGameState = { gs | hand = hs, spike = ss}
    in
    ( { model 
      | gameObjects = [ ss, hs]
      , ticks = model.ticks + 1
      , fps = 1000 / delta
      , gameState = nextGameState }
    , Cmd.none)

collisionDetection : (Model, Cmd msg) -> (Model, Cmd msg) 
collisionDetection (model, msg) = 
    let
        handbb = getBoundingBox model.gameState.hand
        spikebb = getBoundingBox model.gameState.spike

        dead = 
            if model.gameState.dead then True else itCollides handbb spikebb 

        setGameState : GameState -> GameState
        setGameState state =
            { state | dead = dead} 
           
    in
    ({ model | gameState = setGameState model.gameState } , msg)
                    
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Tick delta ->
            ticker delta model
            |> collisionDetection
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

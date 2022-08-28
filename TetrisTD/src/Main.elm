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
import DrGame exposing(..)
import List exposing (foldl)
import DrGrid exposing (..)


type alias GameState =
    { hand: GameObject
    , dead: Bool
    }

type Msg =
    Tick Float
    | KeyMsg Keyboard.Msg
      
type alias Model =
    {  ticks: Int
     , fps: Float
     , oscillate: Int
     , pressedKeys: List Key
     , gameState: GameState
     , enemies: List GameObject
     , traps: List GameObject
     , cooldown: Int
     , floor: List GameObject
     , bullets: List GameObject
     , gameGrid: List GameGridObject 
     , snake: List GameGridObject }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
        

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


drawGameObjects : Model -> List (Html.Html msg)
drawGameObjects model =
    List.map ( \ ft -> draw ft.gameObject) model.gameGrid  
    ++ List.map ( \ f -> draw f) model.floor
    ++ List.map ( \ s -> draw s ) model.traps
    ++ List.map ( \ b -> draw b) model.bullets
    ++ draw model.gameState.hand 
    :: List.map ( \ n -> draw n ) model.enemies
    ++ List.map ( \ s -> draw s.gameObject ) model.snake

        
view : Model -> Html.Html msg
view model =
    div []
        ([ div [] [ text "hello world" ]
        , div [] [ text
                      <| "fps: " ++ String.fromFloat model.fps ]
        , div [] [ text
                       <| "ticks: " ++ String.fromInt model.ticks ]
        ]
             ++ drawGameObjects model
             ++ if model.gameState.dead then 
                    [div [][text "YOU ARE DEAD"]]
                else 
                    [div [][]]
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

knight : Sprite
knight =
    DrSprite.getSprite 192 68 16 28 4

    
        
init : a -> (Model, Cmd msg)
init _ =
    let
        tiles : List GameObject
        tiles =
           fold2d
               {rows = 2, cols = 9}
               (\ (x, y) result ->
                    { sprite = ladderSprite
                    , x = x * 16 * scale 
                    , y = y * 16 * scale + 500
                    , dir = 1
                    } :: result )
               []
    in
    ( { ticks = 1
      , fps = 0.0
      , oscillate = 0
      , pressedKeys = []
      , gameState =
            { hand = 
                { sprite = lizzardSprite 
                , x = 0
                , y = 0
                , dir = 1
                }
            , dead = False
            }
        , enemies = [] 
        , traps = []
        , cooldown = 0
        , floor = tiles
        , bullets = []
        , gameGrid =  DrSprite.getSprite 16 64 16 16 1 |> drawMap 200
        , snake =
             [{ gameObject =
                    { sprite = lizzardSprite
                    , x = 0
                    , y = 0
                    , dir = 1
                    }
               , grid = (0, 0)
               , travelling = Nothing
              }]
      }
        
      , Cmd.none
    )


checkCollisions : List GameObject -> GameObject -> Bool
checkCollisions listgo go =
    let
        bb = getBoundingBox go
    in
    foldl (\trap accum-> 
               let
                   tbb = getBoundingBox trap
               in
               if accum then True else itCollides bb tbb 
          )
          False
          listgo

    --------- UPDATE --------
    --------- UPDATE --------

ticker : Float -> Model -> ( Model, Cmd msg)
ticker delta model =
    let

      pmovespeed = 3
                   
      newM = 
        if model.oscillate > 600 then
            {model | oscillate = 0 }
        else 
            {model | oscillate = model.oscillate + 1 }

      (kx, ky ) = 
        moveOnKeyBoard newM.pressedKeys
        
      (keyx, keyy ) = 
          (kx * pmovespeed, ky * pmovespeed)

      handWillCollide =
          moveGameObject keyx keyy model.gameState.hand
          |> checkCollisions model.floor

      hs = 
        animateGameObject newM.gameState.hand model.ticks
        |> (\go ->
                if handWillCollide then
                    go
                else
                    moveGameObject keyx keyy go)

      gs =
         newM.gameState

      nextEnemmies = 
        if modBy 100 newM.ticks /= 0 then
                newM
            else
                { newM | enemies =  
                    { sprite = knight
                    , x = 100
                    , y = model.oscillate 
                    , dir = 1
                    } :: newM.enemies }

      removeEnemies = 
        List.foldl (\ e a -> 
                if e.x > 600 then
                    a
                else 
                    e :: a
            ) [] nextEnemmies.enemies


      checkEnemyCollision =
        List.foldl ( \ enemy  acc-> 
            let
                bb = getBoundingBox enemy
                collision = foldl (\trap accum-> 
                        let
                            tbb = getBoundingBox trap
                        in
                        if accum then True else itCollides bb tbb 
                    ) False model.traps
            in
            if collision then acc else enemy :: acc 
            ) [] removeEnemies
        
       
      moveEnemies = 
        List.map (\n -> { n| x = n.x + 1 }) checkEnemyCollision

      aniene = List.map (\e -> animateGameObject e model.ticks) moveEnemies

      (traps, cooldown) = 
        if spacebarKlick model.pressedKeys  && model.cooldown < 0 then 
            ({ sprite = floorSpikes
                , x = model.gameState.hand.x
                , y = model.gameState.hand.y
                , dir = 1
            } :: nextEnemmies.traps, 100)
        else 
            (nextEnemmies.traps, model.cooldown - 1)

      aniTraps = 
        traps
        |> List.map (\e -> animateGameObject e model.ticks) 


      handleBullets b =
          if ctrClick model.pressedKeys && cooldown < 0 then
             ({ sprite = DrSprite.getSprite 288 224 16 16 1
             , x = model.gameState.hand.x
             , y = model.gameState.hand.y
             , dir = 1
             } :: b, 100)
          else
             ( b, cooldown - 1)


      
      (createdBullets, nextCooldown)  =
          handleBullets model.bullets

      movedBullets : List GameObject -> List GameObject
      movedBullets b =
          List.map (\ bi -> moveGameObject 1 1 bi) createdBullets 
          |> List.foldl (\ o a ->
                             if o.x > 500 || o.y > 500 then
                                 a
                             else 
                                 o :: a
                        )
                 []
                 
      nextGameState = 
        { gs 
          | hand = hs }
    in
    ( { nextEnemmies 
      | ticks = model.ticks + 1
      , fps = 1000 / delta
      , gameState = nextGameState
      , enemies = aniene 
      , traps = aniTraps
      , cooldown = nextCooldown
      , bullets = movedBullets createdBullets }
    , Cmd.none)

                    
update : Msg -> Model -> (Model, Cmd msg)
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

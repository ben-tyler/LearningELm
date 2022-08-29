module DrGrid exposing (..)

import DrSprite
import DrGame

offset = 100

type alias GameGridObject =
    { gameObject: DrGame.GameObject
    , grid: (Int, Int)
    , travelling: Maybe (Int, Int)
    }

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


gameGrid : DrSprite.Sprite -> List GameGridObject 
gameGrid  sprite =
    fold2d
        {rows = 9, cols = 9}
        (\ (x, y) result ->
             { gameObject =
                   { sprite = sprite  
                   , x = x * 16 * DrGame.scale + offset
                   , y = y * 16 * DrGame.scale + offset
                   , dir = 1
                   }
              , grid = (x, y)
              , travelling = Nothing 
             }:: result)
        []
moveGameGridObject : (Int, Int) -> GameGridObject -> GameGridObject
moveGameGridObject (x, y) ggo = 
    let
        go = ggo.gameObject
    in 
    { ggo 
    | grid = (x, y)
    , gameObject = 
        { go 
        | x = x * 16 * DrGame.scale + offset
        , y = y * 16 * DrGame.scale + offset
        } 
    }


gameGridObject (x, y) sprite = 
    { gameObject =
          { sprite = sprite  
          , x = x * 16 * DrGame.scale + offset
          , y = y * 16 * DrGame.scale + offset
          , dir = 1
          }
    , grid = (x, y)
    , travelling = Nothing
    }
    

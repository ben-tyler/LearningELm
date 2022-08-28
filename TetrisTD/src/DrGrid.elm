module DrGrid exposing (..)

import DrSprite
import DrGame
        
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


gameGrid : Int ->  DrSprite.Sprite -> List GameGridObject 
gameGrid offset sprite =
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


gameGridObject (x, y)  sprite = 
    { gameObject =
          { sprite = sprite  
          , x = x * 16 * DrGame.scale + offset
          , y = y * 16 * DrGame.scale + offset
          , dir = 1
          }
    , grid = (x, y)
    , travelling = Nothing
    }
    

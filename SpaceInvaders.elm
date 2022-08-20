module SpaceInvaders exposing (..)

import Playground exposing (..)

type alias BoundingBox =
    { x: Number
    , y: Number
    , w: Number
    , h: Number
    }

getBoundingBox x y =
    { x = x, y = y, w = 20, h = 20 }

bottom = -200
top = 200
top2 = 250
main =
  game view update
    { x = 0
    , bullets = []
    , invadors = [ (0, top, True)
                 , (50, top, True)
                 , (100, top, True)
                 , (150, top, True)
                 , (200, top, True)
                 , (250, top, True)
                 , (0, top2, True)
                 , (50, top2, True)
                 , (100, top2, True)
                 , (150, top2, True)
                 , (200, top2, True)
                 , (250, top2, True)
                 ]
   ,  enemyBullets = []
    , dead = False }

viewBullet (x, y) =
    square red 9
        |> move x y

updateBullet (x, y) =
    (x, (y+1))

updateEnemyBullet (x, y) =
    (x, (y-1))
        
viewInvador (x, y, alive) =
    if alive then 
        words black "🛸"
            |> move x y
    else
        words black "x"
            |> move x y

viewBoundingBoxes foo =
    let 
        boundingBoxes = List.map (\ (x, y) -> getBoundingBox x y ) foo
    in
        List.map (\ b -> rectangle grey b.w b.h
                      |> move b.x b.y )
            boundingBoxes

invadorsToXY invadors =
    List.map (\ (x, y, isAlive) -> (x, y)) invadors
               
view computer memory =
  [ rectangle white computer.screen.width computer.screen.height
  , words black "🚀"
      |> move memory.x -200
  ]
  ++ List.map viewBullet memory.bullets
  ++ List.map viewInvador memory.invadors
  ++ List.map viewBullet memory.enemyBullets
  ++ if memory.dead then [words red "Dead"] else []
--++ viewBoundingBoxes memory.bullets
  --++ viewBoundingBoxes (invadorsToXY memory.invadors)


updateInvador time (x, y, alive) =
    (x + (zigzag -2 2 4 time), y-0.1, alive)
    

checkCollision b1 b2 =
    if b1.x < b2.x + b2.w &&
        b1.x + b1.w > b2.x &&
        b1.y < b2.y + b2.h &&  
        b1.h + b1.y > b2.y then
        True
     else
        False


removeBullets bullets screenheight =
    List.foldl (\ (x, y) a ->
                    if y > screenheight || y < -250 then
                       a
                    else
                       (x, y) :: a
               ) [] bullets
                    
maybeCreateBullet spacebar bullets x =
    if spacebar then 
        (x, bottom) :: bullets
    else
        bullets


createEnemyBulletes time enemyBullets invadors =
    List.foldl (\ (ix, iy, alive) newBullets ->
      if (wave 0 1 7 time > 0.99) && alive then
         (ix, iy) :: newBullets
       else
           newBullets
                    ) enemyBullets invadors
     
           

checkIfDead x enemyBullets dead =
           let
               bb = List.map (\(bx, by) ->  getBoundingBox bx by) enemyBullets
               playerB = getBoundingBox x bottom 
                
               isDead =
                   if dead then True else 
                       List.foldl (\ bulletBox res ->
                                       if res then
                                           True
                                       else if checkCollision bulletBox playerB then
                                            True
                                       else
                                            False
                               )
                               False
                               bb
             in
                 isDead
            

            
update computer memory =
   let
       removedBullets =
           removeBullets memory.bullets computer.screen.height 

       newBullets =
           maybeCreateBullet
               computer.keyboard.space
               removedBullets
               memory.x

       timeLambda =
           \i -> updateInvador computer.time i

       collisionDetection (ix, iy, ialive) =
           let
               bb = List.map (\(x, y) ->  getBoundingBox x y) memory.bullets
               ib = getBoundingBox ix iy
               aliveResult = List.foldl (\ bulletBox alive ->
                                  if not alive then False
                                  else if checkCollision bulletBox ib then False
                                  else True ) ialive bb           
           in 
           (ix, iy, aliveResult)

       enemyBullets =
           List.map updateEnemyBullet
               (createEnemyBulletes computer.time
                    memory.enemyBullets
                    memory.invadors)
       nextEnemyBullets = removeBullets enemyBullets computer.screen.height

           
  in               
  { x = memory.x + toX computer.keyboard
  , bullets = List.map updateBullet newBullets
  , invadors = List.map timeLambda (List.map collisionDetection memory.invadors)
  , enemyBullets = nextEnemyBullets
  , dead = checkIfDead memory.x memory.enemyBullets memory.dead
  }

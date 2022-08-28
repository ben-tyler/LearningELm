module DrGame exposing (..)

import Keyboard exposing (Key(..))
import Keyboard.Arrows
import DrSprite exposing (..)

scale : number
scale = 3

toModBy : number
toModBy = 10


type alias GameObject =
    { sprite: DrSprite.Sprite
    , x: Int
    , y: Int
    , dir: Int
    }

animateSprite : Sprite -> Sprite
animateSprite sprite =
    { sprite | currentFrame = 
        if sprite.currentFrame == sprite.frames then
                1
            else
                sprite.currentFrame + 1 
    }

animateGameObject : GameObject -> Int -> GameObject
animateGameObject gameObject ticks =
    if modBy toModBy ticks /= 0 then
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

spacebarKlick : List Keyboard.Key -> Bool
spacebarKlick pressedKeys = 
    List.member Spacebar pressedKeys


ctrClick : List Keyboard.Key -> Bool
ctrClick pressedKeys =
    List.member Control pressedKeys
        
        
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
                 , dir = if keyx < 0 then -1 else 1}

module DrGame exposing (..)

import DrSprite exposing (..)
import Keyboard exposing (Key(..))
import Keyboard.Arrows


scale : number
scale =
    3


toModBy : number
toModBy =
    25


type alias GameObject =
    { sprite : DrSprite.Sprite
    , x : Int
    , y : Int
    , dir : Int
    }


type Action = PickUpBox | NoAction | PutDownBox
type alias Intelligence =
    { move : List ( Int, Int, Action )
    , speak: List String
    }


type alias GO =
    { spriteControl : Animation
    , x : Int
    , y : Int
    , dir : Int
    , gameGrid : Maybe ( Int, Int )
    , traveling : Maybe Bool
    , intelligence : Maybe Intelligence -- it is probably a bad idea to couple intelligence here
    }


animateSprite : Sprite -> Sprite
animateSprite sprite =
    { sprite
        | currentFrame =
            if sprite.currentFrame == sprite.frames then
                1

            else
                sprite.currentFrame + 1
    }


animationControl : Animation -> Animation
animationControl animation =
    case animation.current of
        Run ->
            { animation | run = animateSprite animation.run }

        Idle ->
            { animation | idle = animateSprite animation.idle }


getSprite animation =
    case animation.current of
        Run ->
            animation.run

        Idle ->
            animation.idle


animateGO : GO -> Int -> AnimationTypes -> GO
animateGO go ticks ani =
    let
        controlSprite spriteControl =
            case ani of
                Run ->
                    { spriteControl
                        | run = animateSprite spriteControl.run
                        , current = ani
                    }

                Idle ->
                    { spriteControl
                        | idle = animateSprite spriteControl.idle
                        , current = ani
                    }
    in
    if modBy toModBy ticks /= 0 then
        go

    else
        { go | spriteControl = controlSprite go.spriteControl }


animateGameObject : GameObject -> Int -> GameObject
animateGameObject gameObject ticks =
    if modBy toModBy ticks /= 0 then
        gameObject

    else
        { gameObject | sprite = animateSprite gameObject.sprite }


moveOnKeyBoard : List Keyboard.Key -> ( Int, Int )
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


goBoundingBox : GO -> Float -> BoundingBox
goBoundingBox go scales =
    { x = go.x
    , y = go.y
    , w = round (go.spriteControl.idle.w * scales) --gameObject.sprite.w * scale)
    , h = round (go.spriteControl.idle.h * scales) --round (gameObject.sprite.h * scale)
    }


itCollides : BoundingBox -> BoundingBox -> Bool
itCollides b1 b2 =
    b1.x < b2.x + b2.w && b1.x + b1.w > b2.x && b1.y < b2.y + b2.h && b1.h + b1.y > b2.y


moveGameObject : Int -> Int -> GameObject -> GameObject
moveGameObject keyx keyy gameObject =
    { gameObject
        | x = gameObject.x + keyx
        , y = gameObject.y + keyy
        , dir =
            if keyx < 0 then
                -1

            else
                1
    }


moveGO : Int -> Int -> GO -> GO
moveGO keyx keyy gameObject =
    let
        handleSpriteControl spriteControl =
            case ( spriteControl.current, keyx, keyy ) of
                ( Run, 0, 0 ) ->
                    { spriteControl | current = Idle }

                ( Idle, _, _ ) ->
                    { spriteControl | current = Run }

                ( _, _, _ ) ->
                    spriteControl
    in
    { gameObject
        | x = gameObject.x + keyx
        , y = gameObject.y + keyy
        , dir =
            if keyx < 0 then
                -1

            else
                1

        -- , spriteControl = handleSpriteControl gameObject.spriteControl
    }
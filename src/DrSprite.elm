module DrSprite exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

type AnimationTypes = Run | Idle

type alias Animation = 
    { current: AnimationTypes
    , run: Sprite 
    , idle: Sprite
    }

type alias Sprite =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , frames : Float
    , currentFrame : Float
    }


getSprite : Float -> Float -> Float -> Float -> Float -> Sprite
getSprite x y w h f =
    { x = x
    , y = y
    , w = w
    , h = h
    , frames = f
    , currentFrame = 1
    }


drawPosition : ( Float, Float ) -> List (Html.Attribute msg)
drawPosition ( x, y ) =
    [ style "position" "absolute"
    , style "left" <| String.fromFloat x ++ "px"
    , style "top" <| String.fromFloat y ++ "px"
    ]


scaleSprite : Float -> String -> String
scaleSprite factor direction =
    "scale("
        ++ String.fromFloat factor
        ++ ") scaleX("
        ++ direction
        ++ ")"


setSpriteSheet : Sprite -> String
setSpriteSheet sprite =
    "-" ++ String.fromFloat (sprite.x + sprite.currentFrame * sprite.w) ++ "px" ++ " -" ++ String.fromFloat sprite.y ++ "px"


viewSprite : String -> Float -> Sprite -> Float -> ( Float, Float ) -> Html msg
viewSprite spriteSheet scale sprite dir ( xloc, yloc ) =
    let
        spriteDirection =
            if dir == 0 then
                String.fromFloat 1

            else
                String.fromFloat dir
    in
    div (drawPosition ( xloc, yloc ))
        [ div
            [ style "background" <| "url(" ++ spriteSheet ++ ") no-repeat"
            , style "background-position" <| setSpriteSheet sprite
            , style "width" <| String.fromFloat sprite.w ++ "px"
            , style "height" <| String.fromFloat sprite.h ++ "px"
            , style "transform" <| scaleSprite scale spriteDirection
            ]
            []
        ]
module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, text)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Time
import Browser.Events exposing (onAnimationFrame)
import Html.Attributes exposing (style)
import Random

dungeonSpriteSheet = "0x72_DungeonTilesetII_v1.4.png"
scaleConstant = 2


itCollides : GameObject -> GameObject -> Bool
itCollides b1 b2 =
    let
        (b1x, b1y )= b1.pos
        (b2x, b2y) = b2.pos
        widthCollission = b1x < b2x + b2.w && b1x + b1.w > b2x
        heightCollision = b1y < b2y + b2.h && b1.h + b1y > b2y
    in
    widthCollission && heightCollision


doubleList : (GameObject, Sprite) -> List (GameObject, Sprite) -> List (GameObject, Sprite)
doubleList (toon, toons) inputList =
    case inputList of
        [] ->
            [] 

        (xgo, xgs) :: xs ->
            if itCollides toon xgo then 
                doubleList (toon, toons) xs
            else
                (xgo, xgs) :: doubleList (toon, toons) xs

pickupExperienceDiamond : Model -> Model
pickupExperienceDiamond model = 
    { model | experianceDiamond = doubleList model.knight model.experianceDiamond }

type Msg = 
    Tick Float
    | LockedTick Time.Posix
    | KeyMsg Keyboard.Msg
    | AddDiamond Int
      
type alias GameObject = 
    { pos : (Float, Float)
    , dir : Int
    , w : Float
    , h : Float
    }

type alias Model =
    { ticks: Int
     , fps: Float
     , pressedKeys: List Key
     , knight :  (GameObject, Sprite)
     , experianceDiamond : List (GameObject, Sprite)
     }

init : a -> (Model, Cmd Msg)
init _ =
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , knight = 
        ( GameObject (100, 100) 0 20 20
        , Sprite 192 68 16 28 4 0
        )
      , experianceDiamond = []
      }
      , Cmd.batch
        [ Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        ]
    )



addDiamond : Model -> Int -> Model
addDiamond model randomNumber = 
    { model 
        | experianceDiamond = 
            ( GameObject (toFloat randomNumber, toFloat randomNumber) 0 20 20
            , Sprite 288 320 16 16 1 0
            ) :: model.experianceDiamond 
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , onAnimationFrame LockedTick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]                 
       
viewGameObject (gameobject, sprite) =
    viewSprite 
        dungeonSpriteSheet 
        scaleConstant 
        sprite 
        ( gameobject.dir |> toFloat )
        gameobject.pos

view : Model -> Html.Html msg
view model =
    div []
        [ viewGameObject model.knight
       ,  div [] (List.map viewGameObject model.experianceDiamond )
        , div [] [ text
                       <| "fps: " ++ String.fromFloat model.fps ]
        , div [] [ text
                       <| "ticks: " ++ String.fromInt model.ticks ]
        , div [] [ text (Debug.toString model)]     
        ]

animate (go, sprite) = 
    if sprite.currentFrame == 4 then 
        (go, { sprite | currentFrame = 0 })
    else
       (go, { sprite | currentFrame = sprite.currentFrame + 1 })

handleFramerate : Int -> Bool
handleFramerate ticks = 
    modBy 5 ticks == 0

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        AddDiamond randomNumber -> 
            (addDiamond model randomNumber, Cmd.none)

        Tick _ -> 
            ( { model | ticks = model.ticks + 1 }, Cmd.none )
        
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys}, Cmd.none )
        
        LockedTick posix -> 
            if handleFramerate model.ticks then 
                ( { model 
                    | ticks = model.ticks + 1 
                    , knight = 
                        animate model.knight
                            |> moveOnKeyBoard model.pressedKeys
                  }
                    |> pickupExperienceDiamond
                , Cmd.none
                )
            else 
                (model, Cmd.none)


moveOnKeyBoard : List Keyboard.Key -> (GameObject, Sprite) -> (GameObject, Sprite)
moveOnKeyBoard pressedKeys (go, sprite) =
    let
        arrows =
            Keyboard.Arrows.arrows pressedKeys

        (cx, cy) = go.pos
        newGo = 
            { go 
                | pos =  
                    ( ( arrows.x * 20 |> toFloat) + cx 
                    , ( arrows.y * -1 * 20 |> toFloat) + cy
                    )
                , dir = if arrows.x == 0 then go.dir else arrows.x
            }
    in
    (newGo, sprite)
   


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Sprite =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , frames : Float
    , currentFrame : Float
    }

setSpriteSheet : Sprite -> String
setSpriteSheet sprite =
    "-" ++ String.fromFloat (sprite.x + sprite.currentFrame * sprite.w) ++ "px" ++ " -" ++ String.fromFloat sprite.y ++ "px"


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
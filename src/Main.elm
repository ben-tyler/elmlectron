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
import Time exposing (millisToPosix)
import Html.Attributes exposing (width)
import Platform.Cmd as Cmd
import Dict exposing (remove)

dungeonSpriteSheet : String
dungeonSpriteSheet = "0x72_DungeonTilesetII_v1.4.png"
scaleConstant : Float
scaleConstant = 2

type Msg = 
    Tick Float
    | LockedTick Time.Posix
    | KeyMsg Keyboard.Msg
    | AddDiamond Int
    | AddEnemy Int
      
type alias GameObject = 
    { pos : (Float, Float)
    , dir : Int
    , w : Float
    , h : Float
    }

type alias SpriteGameObject = 
    { go: GameObject
    , sp: Sprite
    , id: Int
    }

type alias Model =
    { ticks: Int
     , lastPosixTime : Time.Posix
     , fps : Float
     , pressedKeys: List Key
     , knight :  SpriteGameObject
     , experience : Int
     , experianceDiamond : List SpriteGameObject
     , bullets : List SpriteGameObject
     , enemies : List SpriteGameObject
     }

     

init : a -> (Model, Cmd Msg)
init _ =
    ( { ticks = 1
      , lastPosixTime = (millisToPosix 0 )
      , fps = 0
      , pressedKeys = []
      , knight = 
            SpriteGameObject
                (GameObject (100, 100) 1 20 20)
                (Sprite 192 68 16 28 4 0)
                1
      , experience = 0
      , experianceDiamond = []
      , bullets = []
      , enemies = []
      }
      , Cmd.batch
        [ Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        , Random.generate AddDiamond (Random.int 1 400)
        ]
    )



addDiamond : Model -> Int -> Model
addDiamond model randomNumber = 
    { model 
        | experianceDiamond = 
            (SpriteGameObject
                ( GameObject (toFloat randomNumber, toFloat randomNumber) 0 20 20)
                ( Sprite 288 320 16 16 1 0 )
                (List.length model.experianceDiamond + 1)
            ) :: model.experianceDiamond 
    }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , onAnimationFrame LockedTick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]                 
       
viewGameObject spriteGameObject =
    viewSprite 
        dungeonSpriteSheet 
        scaleConstant 
        spriteGameObject.sp 
        ( spriteGameObject.go.dir |> toFloat )
        spriteGameObject.go.pos

view : Model -> Html.Html msg
view model =
    div [ style "position" "relative"
        , style "overflow" "hidden"
        , style "width" "700px"
        , style "height" "500px"
        ]
        [ viewGameObject model.knight
        , div [] (List.map viewGameObject model.experianceDiamond )
        , div [] (List.map viewGameObject model.bullets )
        , div [] (List.map viewGameObject model.enemies)
        , div [] [ text <| "Experience " ++ String.fromInt model.experience]
        , div [] [ text <| "FPS " ++ Debug.toString model.fps ]
       -- , div [] [ text <| Debug.toString model.enemies]
        ]
animate : SpriteGameObject -> SpriteGameObject
animate spgo = 
    let
        sprite = spgo.sp
    in
    
    if spgo.sp.currentFrame == 4 then 
        { spgo| sp = { sprite | currentFrame = 0} }
    else
        { spgo| sp = { sprite | currentFrame = sprite.currentFrame + 1} }

handleFramerate : Int -> Int -> Bool
handleFramerate ticks speed = 
    modBy speed ticks == 0



itCollides : GameObject -> GameObject -> Bool
itCollides b1 b2 =
    let
        (b1x, b1y )= b1.pos
        (b2x, b2y) = b2.pos
        widthCollission = b1x < b2x + b2.w && b1x + b1.w > b2x
        heightCollision = b1y < b2y + b2.h && b1.h + b1y > b2y
    in
    widthCollission && heightCollision

getRemoved : List SpriteGameObject -> List SpriteGameObject -> List SpriteGameObject -> List SpriteGameObject
getRemoved oldList newList removed = 
    case oldList of 
        [] -> removed
        (x::xs) -> 
            let
                itemRemains = List.any ( \ i -> i.id == x.id ) newList
            in
            if not itemRemains then 
                getRemoved xs newList (x :: removed)
            else
                getRemoved xs newList removed



removeIfCollides : SpriteGameObject -> List SpriteGameObject -> List SpriteGameObject
removeIfCollides sgo inputList =
    case inputList of
        [] ->
            [] 

        xsgo :: xs ->
            if itCollides sgo.go xsgo.go then 
                removeIfCollides sgo xs
            else
                xsgo :: removeIfCollides sgo xs

pickupExperienceDiamond : Model -> Model
pickupExperienceDiamond model = 
    let 
        afterPickup = removeIfCollides model.knight model.experianceDiamond
        experienceGained = List.length model.experianceDiamond - List.length afterPickup
    in
    { model 
        | experianceDiamond = afterPickup 
        , experience = model.experience + experienceGained
    }

moveBullet : SpriteGameObject -> SpriteGameObject
moveBullet spgo =
    let
        (x, y) = spgo.go.pos
        nx = (spgo.go.dir * 10 |> toFloat) + x

        spgoGO = spgo.go
        nextGo = {spgoGO | pos = (nx, y) }
    in
    {spgo | go = nextGo }  


killEnemies : List SpriteGameObject -> List SpriteGameObject -> List SpriteGameObject
killEnemies bullets enemies = 
    case (enemies, bullets) of
        ([], _) -> 
            enemies
        (_, []) -> 
            enemies
        (e, x::xs) -> 
            killEnemies xs (removeIfCollides x e)



shootBullets : Model -> Model
shootBullets model = 
    let
        toon = model.knight
        nextModel = 
            { model 
                | bullets = List.map moveBullet model.bullets
                , enemies = killEnemies model.bullets model.enemies
            }

        deadEnemies = getRemoved model.enemies nextModel.enemies []

        newExperienceDiamonds = 
            List.indexedMap
                ( \ index i -> 
                    (SpriteGameObject
                        ( GameObject i.go.pos 0 20 20)
                        ( Sprite 288 320 16 16 1 0 )
                        ((List.length model.experianceDiamond) + 1 + index)
                    )
                )
                deadEnemies

        modelWithXp = { nextModel | experianceDiamond = newExperienceDiamonds ++ nextModel.experianceDiamond }
        
    in
    if  modBy 50 model.ticks == 0 then 
        { modelWithXp 
            | bullets = 
                (SpriteGameObject 
                    (GameObject toon.go.pos toon.go.dir 20 20)
                    (Sprite 336 224 16 16 0 0)
                    (List.length model.bullets + 1)
                ) :: model.bullets 
        }
    else
        modelWithXp

calculateFPS : Time.Posix -> Time.Posix -> Float
calculateFPS timestamp1 timestamp2 =
    let
        timeDifferenceInSeconds =
            ((Time.posixToMillis timestamp2) - (Time.posixToMillis timestamp1) |> toFloat ) / 1000.0
            -- Divide by 1000 to convert milliseconds to seconds
        frames = 1
        fps = frames / timeDifferenceInSeconds
    in
    fps

controllPlayer : Model -> Model 
controllPlayer model = 
    let
        newKnight = 
            if handleFramerate model.ticks 5 then 
                animate model.knight 
            else
                model.knight

    in
    { model | knight = newKnight |> moveOnKeyBoard model.pressedKeys }


moveTowards : SpriteGameObject -> SpriteGameObject -> SpriteGameObject
moveTowards towards mover = 
    let
        (twowardsPosX, towardsPosY) = towards.go.pos
        (movePosX, movePosY) = mover.go.pos
        npx = if twowardsPosX > movePosX then movePosX + 1 else movePosX - 1
        npy = if towardsPosY > movePosY then movePosY + 1 else movePosY - 1
        go = mover.go
    in
    { mover | go = { go | pos = (npx, npy)}}

moveEnemies : Model -> Model
moveEnemies model = 
    if handleFramerate model.ticks 5 then 
        { model | enemies = List.map animate model.enemies }
    else
        { model | enemies = List.map (moveTowards model.knight) model.enemies}


updateMetaData : Time.Posix -> Model -> Model 
updateMetaData posix model = 
    { model 
      | ticks = model.ticks + 1 
      , lastPosixTime = posix
      , fps = calculateFPS posix model.lastPosixTime
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddEnemy randomX -> 
            ({ model 
                | enemies = 
                    [ SpriteGameObject
                        (GameObject (randomX |> toFloat, 0) 1 20 20)
                        ( Sprite 144 364 32 36 4 0 )
                        (List.length model.enemies + 1)
                    , SpriteGameObject
                        (GameObject (randomX |> toFloat, 400) 1 20 20)
                        ( Sprite 144 364 32 36 4 0 )
                        (List.length model.enemies + 2)
                    ] ++ model.enemies 
            }
            , Cmd.none)

        AddDiamond randomNumber -> 
            (addDiamond model randomNumber, Cmd.none)

        Tick _ -> 
            ( model, Cmd.none )
        
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys}, Cmd.none )
        
        LockedTick posix -> 
            ( model 
                |> updateMetaData posix
                |> controllPlayer
                |> pickupExperienceDiamond
                |> shootBullets
                |> moveEnemies

            ,   if handleFramerate model.ticks 200 then 
                    Random.generate AddEnemy (Random.int 1 400)
                else
                    Cmd.none
            )



moveOnKeyBoard : List Keyboard.Key -> SpriteGameObject -> SpriteGameObject
moveOnKeyBoard pressedKeys spriteGameObject =
    let
        arrows =
            Keyboard.Arrows.arrows pressedKeys

        (cx, cy) = spriteGameObject.go.pos
        go = spriteGameObject.go
        newXPos = ( arrows.x * 2 |> toFloat) + cx 
        newYPos = ( arrows.y * -1 * 2 |> toFloat) + cy
            
    in
    { spriteGameObject
        | go = 
            { go 
                |pos = (newXPos, newYPos)
                , dir = if arrows.x == 0 then go.dir else arrows.x
            }
    }
   


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
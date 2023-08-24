module Main exposing (..)

import Keyboard
import DrGame exposing (GameObject)
import DrGrid exposing (GameGridObject)
import DrSprite exposing (getSprite)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onAnimationFrame)
import Html exposing (div, img, text)
import Html.Attributes exposing (height, src, style, width)
import DrGrid exposing (gameGridObject)
import Time

----------Move this
scale = 3

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



-----------

type Msg = 
    Tick Float
    | LockedTock Time.Posix
    | KeyMsg Keyboard.Msg

type alias Model =
    { ticks: Int
    , fps: Float 
    , pressedKeys: List Keyboard.Key
    , gameGrid: List GameGridObject
    , snake: List GameGridObject
    , food: List GameGridObject
    , direction: (Int, Int)
    }

makeSnake : (Int, Int) -> GameGridObject
makeSnake (x, y) = 
            getSprite 192 68 16 28 4
            |> DrGrid.gameGridObject (x, y)

init : a -> (Model, Cmd msg)
init _ =
    let
        food loc = 
            getSprite 288 224 16 16 1
            |> DrGrid.gameGridObject loc
    in
    ( { ticks = 1
      , fps = 0.0
      , pressedKeys = []
      , gameGrid = getSprite 16 16 16 16 1
                   |> DrGrid.gameGrid
      , snake = [ makeSnake (1, 0), makeSnake (0, 0)]
      , food = 
        [ food (3, 0)
        , food (5, 0)
        , food (3, 3)
        , food (0, 0)
        , food (6, 6)
        , food (8, 0)
        , food (0, 8)
        , food (2, 5)
        , food (6, 3)
        , food (5, 5)
        , food (8, 2)
        ]
      , direction = (1, 0)
      }
    , Cmd.none
    )

view : Model -> Html.Html msg

view model =
    let 
        drawFood = 
            case model.food of 
                x::_ -> [draw x.gameObject]
                [] -> [div [] []]
    in   
    div []
        (  div [] [ text <| String.fromInt model.ticks ] 
           :: List.map (\i -> draw i.gameObject) model.gameGrid 
           ++ drawFood
          -- ++ List.map (\i -> draw i.gameObject ) model.food
           ++ List.map( \i -> draw i.gameObject) model.snake
        )

handleSnake : Model -> Model
handleSnake model =
    let
        _ = Debug.log "snek" (List.length model.snake)
        getCurrentFoodLocation = 
            case model.food of
                f::_ -> f.grid
                [] -> (99, 99)

        (dx, dy)= model.direction

        newLoc (x, y) = 
            (x+ dx, y + dy)
 
        move ggo = 
            DrGrid.smoothSetGoo 
                (newLoc ggo.grid)
                 ggo

        -- item moves to the head of the snek
        smf: List GameGridObject -> Maybe (Int, Int) -> List GameGridObject -> List GameGridObject
        smf accum prev snake = 
            case snake of
                [] -> accum
                x::xs -> 
                    case prev of
                        Nothing -> smf [move x] (Just x.grid) xs
                        Just ploc -> 
                            smf 
                                (accum ++ [(DrGrid.smoothSetGoo ploc x)])
                                (Just x.grid)
                                xs


        tryToEat snake = 
            case snake of 
                snakeHead::r ->
                    let
                        (tx, ty) = 
                            case List.reverse snake of 
                                x::_ -> x.grid
                                [] -> (9, 9)
                    in
                    if snakeHead.grid == getCurrentFoodLocation then
                        snakeHead :: r ++ [makeSnake (tx - dx, ty - dy)]
                    else 
                        snakeHead::r
                    --(onFood h) ++ r
                [] -> []

        nextFood = 
            if List.length (tryToEat model.snake) /=  List.length model.snake then 
                case model.food of 
                    _::r -> r
                    [] -> []
            else
                model.food 

        
   in
   { model 
   | snake =
         smf [] Nothing model.snake
         |> tryToEat
    , food = nextFood
   -- , direction = (dx, dy)
   }

tick : Float -> Model -> Model
tick delta model =
    let
        (xdir, _) = model.direction
        animated = 
            List.map DrGrid.smoothMoveGgo model.snake
            |> List.map (\ i -> DrGrid.animateGameGridObject i model.ticks)
            |> List.map( \i -> DrGrid.setDir i xdir)

        animatedModel = 
            { model
            | snake = animated
            }

        snakeHandled = handleSnake animatedModel
            
        handleDirection = 
            case DrGame.moveOnKeyBoard model.pressedKeys of 
                (0, 0) -> model.direction
                (_, _) -> DrGame.moveOnKeyBoard model.pressedKeys

    in
    if modBy 200 model.ticks == 0 then
       { snakeHandled
       | direction = handleDirection
       }
    else 
        { animatedModel
        | direction = handleDirection 
        }
        

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        LockedTock posix -> 
            ( tick 
                1.0 
                ({ model | ticks = model.ticks + 1 })
            , Cmd.none
            )

        Tick delta ->
            --( tick 
            --    delta 
            --    ({ model | ticks = model.ticks + 1 })
            --, Cmd.none
            --)
            (model, Cmd.none)
       
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ onAnimationFrameDelta Tick
        , onAnimationFrame LockedTock
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
        
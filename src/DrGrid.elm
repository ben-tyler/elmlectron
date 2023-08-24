module DrGrid exposing (..)

import Debug
import DrGame exposing (..)
import DrSprite
import Html exposing (i)
import Maybe exposing (withDefault)


offset =
    100


type alias GameGridObject =
    { gameObject : DrGame.GameObject
    , grid : ( Int, Int )
    , travelling : Maybe ( Int, Int )
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
gameGrid sprite =
    fold2d
        { rows = 9, cols = 9 }
        (\( x, y ) result ->
            { gameObject =
                { sprite = sprite
                , x = x * 16 * DrGame.scale + offset
                , y = y * 16 * DrGame.scale + offset
                , dir = 1
                }
            , grid = ( x, y )
            , travelling = Nothing
            }
                :: result
        )
        []


moveGameGridObject : ( Int, Int ) -> GameGridObject -> GameGridObject
moveGameGridObject ( x, y ) ggo =
    let
        go =
            ggo.gameObject
    in
    { ggo
        | grid = ( x, y )
        , gameObject =
            { go
                | x = x * 16 * DrGame.scale + offset
                , y = y * 16 * DrGame.scale + offset
            }
    }


animateGameGridObject : GameGridObject -> Int -> GameGridObject
animateGameGridObject ggo ticks =
    { ggo
        | gameObject = DrGame.animateGameObject ggo.gameObject ticks
    }


goSetGrid : ( Int, Int ) -> GO -> GO
goSetGrid ( x, y ) go =
    { go
        | traveling = Just True
        , gameGrid = Just ( x, y )
    }


type Map
    = List DrGame.GO


goMvGrid : List DrGame.GO -> GO -> GO
goMvGrid map go =
    let
        _ =
            Debug.log "logme" map

        getDirection : Maybe GO
        getDirection =
            List.foldl
                (\i a ->
                    case ( i.gameGrid, go.gameGrid ) of
                        ( Just ( ix, iy ), Just ( gx, gy ) ) ->
                            if ix == gx && iy == gy then
                                Just i

                            else
                                a

                        _ ->
                            a
                )
                Nothing
                map

        travel : Maybe GO -> GO
        travel maybeDir =
            let
                _ =
                    Debug.log "dir" maybeDir

                _ =
                    Debug.log "daisy" go
            in
            case maybeDir of
                Nothing ->
                    go

                Just dir ->
                    if go.x == dir.x && go.y == dir.y then
                        { go | traveling = Nothing }

                    else
                        { go
                            | x =
                                if go.x <= dir.x then
                                    go.x + 1

                                else
                                    go.x - 1
                            , y =
                                if go.y <= dir.y then
                                    go.y + 1

                                else
                                    go.y - 1
                            , dir =
                                if go.x <= dir.x then
                                    1

                                else
                                    -1
                        }
    in
    case go.traveling of
        Nothing ->
            go

        _ ->
            travel getDirection


smoothSetGoo ( x, y ) ggo =
    { ggo
        | travelling = Just ( x, y )
        , grid = ( x, y )
    }


smoothMoveGgo ggo =
    let
        go =
            ggo.gameObject

        doTravel : GameGridObject -> ( Int, Int ) -> GameGridObject
        doTravel tggo ( tx, ty ) =
            let
                motTraveling =
                    tggo.gameObject.x
                        == tx
                        * 16
                        * DrGame.scale
                        + offset
                        && tggo.gameObject.x
                        == ty
                        * 16
                        * DrGame.scale
                        + offset

                moveX =
                    if tggo.gameObject.x < tx * 16 * DrGame.scale + offset then
                        1

                    else
                        -1

                moveY =
                    if tggo.gameObject.y < ty * 16 * DrGame.scale + offset then
                        1

                    else
                        -1
            in
            if motTraveling then
                tggo
                --  { tggo
                --  | travelling = Nothing
                --  }

            else
                { tggo
                    | gameObject =
                        { go
                            | x = go.x + moveX
                            , y = go.y + moveY
                        }
                }
    in
    case ggo.travelling of
        Nothing ->
            ggo

        Just ( tx, ty ) ->
            doTravel ggo ( tx, ty )


gameGridObject ( x, y ) sprite =
    { gameObject =
        { sprite = sprite
        , x = x * 16 * DrGame.scale + offset
        , y = y * 16 * DrGame.scale + offset
        , dir = 1
        }
    , grid = ( x, y )
    , travelling = Nothing
    }


setDir ggo dir =
    let
        go =
            ggo.gameObject
    in
    { ggo
        | gameObject =
            { go
                | dir = dir
            }
    }
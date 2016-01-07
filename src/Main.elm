module Main(main) where

import Mouse
import Keyboard
import Html exposing (..)
import Graphics.Element exposing (..)

import Game.Board exposing (..)
import Game.Logic exposing (..)
import View.Board exposing (..)
import Game.BoardCircle exposing (..)

type alias Cursor = (Int, Int)

type alias Arrow = { x : Int, y : Int }


cursorOrigin : Cursor
cursorOrigin =
    (4,4)


cursorCoordinateY : Signal Arrow
cursorCoordinateY =
    Keyboard.arrows


cursorCoordinateX : Signal Arrow
cursorCoordinateX =
    Keyboard.arrows


fromArrowsToCoordinates : Arrow -> Arrow -> Cursor
fromArrowsToCoordinates x y =
    (,) -x.y y.x


cursorCoordinates : Signal Cursor
cursorCoordinates =
    Signal.map2 fromArrowsToCoordinates cursorCoordinateX cursorCoordinateY


updateCursor : Cursor -> Cursor -> Cursor
updateCursor cursorSignal oldCursor =
    let
        oldX = fst oldCursor
        oldY = snd oldCursor

        xDeviation = fst cursorSignal
        yDeviation = snd cursorSignal
    in
        if (oldX == 7 && oldY == 7)
        && (xDeviation == 1 || yDeviation == 1)
        then
            (,)
            (fst oldCursor)
            (snd oldCursor)

        else if (oldX == 0 && oldY == 0)
        && (xDeviation == -1 || yDeviation == -1)
        then
            (,)
            (fst oldCursor)
            (snd oldCursor)

        else if (oldX == 7 && oldY < 7)
        && (xDeviation == 1 )
        then
            (,)
            (fst oldCursor)
            ((snd oldCursor) + (snd cursorSignal))

        else if (oldX < 7 && oldY == 7)
        && (yDeviation == 1 )
        then
            (,)
            ((fst oldCursor) + (fst cursorSignal))
            (snd oldCursor)

        else if (oldX == 0 && oldY > 0)
        && (xDeviation == -1 )
        then
            (,)
            (fst oldCursor)
            ((snd oldCursor) + (snd cursorSignal))

        else if (oldX > 0 && oldY == 0)
        && (yDeviation == -1 )
        then
            (,)
            ((fst oldCursor) + (fst cursorSignal))
            (snd oldCursor)

        else
            (,)
            ((fst oldCursor) + (fst cursorSignal))
            ((snd oldCursor) + (snd cursorSignal))


cursor : Cursor -> Signal Cursor
cursor cursor =
    Signal.foldp updateCursor cursor cursorCoordinates


--boardSignal : Signal Board
--boardSignal =
--    let
--        board = setBoardOrigin <| createBoard
--    in
--        Signal.foldp (\clk board -> makePlay (4,2) East board) board Mouse.clicks 


main: Signal Element
main =
    Signal.map show <| cursor cursorOrigin

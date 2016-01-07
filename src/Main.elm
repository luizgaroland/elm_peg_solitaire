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

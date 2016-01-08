module Controls.Controls where


import Keyboard


import Game.BoardCircle exposing (..)
import Game.Logic exposing (..)


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


getCursorDeviation : Signal Cursor
getCursorDeviation =
    Signal.map2 fromArrowsToCoordinates cursorCoordinateX cursorCoordinateY


updateCursor : Cursor -> Cursor -> Cursor
updateCursor cursorSignal oldCursor =
    let
        oldCursor =
            (,)
            (fst oldCursor)
            (snd oldCursor)

        newCursor =
            (,)
            ((fst oldCursor) + (fst cursorSignal))
            ((snd oldCursor) + (snd cursorSignal))

    in
        if validateCoord newCursor
        then
            newCursor

        else
            oldCursor


getCursor : Signal Cursor
getCursor =
    Signal.foldp updateCursor cursorOrigin getCursorDeviation


fromDeviationToDirection : Cursor -> Direction
fromDeviationToDirection cursor =
    let
        cursorX = fst cursor

        cursorY = snd cursor

    in
        if cursorX == 1 && cursorY == 0 then
            South

        else if cursorX == -1 && cursorY == 0 then
            North

        else if cursorX == 0 && cursorY == 1 then
            East

        else if cursorX == 0 && cursorY == -1 then
            West

        else
            InvalidDirection


makePlayKeyPressed : Signal Bool
makePlayKeyPressed =
    Keyboard.space

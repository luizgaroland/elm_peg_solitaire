module Game.Controls where


import Keyboard


import Game.Definition exposing (..)


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


getCursorDirection : Signal Direction
getCursorDirection =
    Signal.map fromDeviationToDirection getCursorDeviation


makePlayKeyPressed : Signal Bool
makePlayKeyPressed =
    Keyboard.space



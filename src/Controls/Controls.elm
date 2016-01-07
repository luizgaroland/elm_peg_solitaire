module Controls.Controls where


import Keyboard


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
    Signal.foldp updateCursor cursorOrigin cursorCoordinates

module Game.Definition where


import Dict exposing (..)


type Direction = North | South | East | West | InvalidDirection


type GameState = 
    --Game start everything but the center circle with piece
    Origin

    -- Plays are still possible
    | Playing

    -- PLay is happening and player need to input a direction to it happen
    | PlayingToChooseDirection

    -- Plays aren't possible anymore
    -- and there are more than one piece on the board
    | Loss

    -- Plays aren't Possible anymore and theres one piece on the board
    | Win


type alias Row = Int


type alias Board = Dict Coordinate BoardCircle


type alias BoardRow = Dict Coordinate BoardCircle


type alias BoardPosition = Dict Coordinate BoardCircle


type alias Coordinate = (Int, Int)


type alias BoardCircle =
    {
        hasPiece : Bool
    }


type alias Cursor = (Int, Int)


type alias Arrow = { x : Int, y : Int }


type alias Play = Signal ( Bool, Cursor )


type alias Game =
    {
        gameState : GameState
    ,   board : Board
    ,   cursor : Cursor
    }
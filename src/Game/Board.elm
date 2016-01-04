module Game.Board where

import Dict exposing (..)

type alias Coordinate = (Int, Int)


type alias BoardCircle =
    {
        hasPiece : Bool
    }


type alias Board = Dict Coordinate BoardCircle


validateCoord : Coordinate -> Bool
validateCoord coord =
    if ( getX coord > 2 && getX coord < 6 ) 
    && ( getY coord > 0 && getY coord < 4 ) then
        True

    else if ( getX coord > 0 && getX coord < 8 ) 
    && ( getY coord > 2 && getY coord < 6 ) then 
        True

    else if ( getX coord > 2 && getX coord < 6 )
    && ( getY coord > 5 && getY coord < 8 ) then
        True

    else
        False


getTuple : Int -> Int -> (Int, Int)
getTuple x y =
    (x, y)


getTuplesRow : Int -> List (Int, Int)
getTuplesRow digit =
    List.map (getTuple digit) [1..7]


getTuplesList : List (Int, Int)
getTuplesList =
    List.concat <| List.map getTuplesRow [1..7]


getX : Coordinate -> Int
getX tuple =
    fst tuple

getY : Coordinate -> Int
getY tuple =
    snd tuple


boardCircle : Bool -> BoardCircle
boardCircle hpiece =
    {
        hasPiece = hpiece
    }


getValidCoordinates : List Coordinate
getValidCoordinates = 
    List.filter validateCoord getTuplesList


getBoardCircles : List BoardCircle
getBoardCircles =
    let
        fromDigitToCircle digit =
            boardCircle False
    in
    List.map fromDigitToCircle [1..33]


zipCoordWithBoardCircles : List Coordinate -> List BoardCircle -> List ( Coordinate, BoardCircle )
zipCoordWithBoardCircles coords circles =
    List.map2 (,) coords circles


createBoard: Dict Coordinate BoardCircle
createBoard =
    Dict.fromList <| zipCoordWithBoardCircles getValidCoordinates getBoardCircles
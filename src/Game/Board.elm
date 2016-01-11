module Game.Board where

import Dict exposing (..)
import Game.Definition exposing (..)
import Game.BoardCircle exposing (..)


getCoordinatesRow : Int -> List Coordinate
getCoordinatesRow digit =
    List.map ( coordinate digit ) [1..7]


getCoordinatesList : List Coordinate
getCoordinatesList =
    List.concat <| List.map getCoordinatesRow [1..7]


addCoordinateOnColumn : Coordinate -> Int -> Coordinate
addCoordinateOnColumn coordinate num =
    let
        x = (+) (getX coordinate) num

        y = getY coordinate
    in
        (,) x y


addCoordinateOnRow : Coordinate -> Int -> Coordinate
addCoordinateOnRow   coordinate num =
    let
        x = getX coordinate

        y = (+) (getY coordinate) num
    in
        (,) x y


getValidCoordinates : List Coordinate
getValidCoordinates =
    List.filter validateCoord getCoordinatesList


getBoardCircles : List BoardCircle
getBoardCircles =
    let
        fromDigitToCircle digit =
            boardCircle False

    in
        List.map fromDigitToCircle [1..33]


createBoard : Board
createBoard =
    Dict.fromList <| zipCoordWithBoardCircles
    getValidCoordinates getBoardCircles


setBoardPiece : Coordinate -> Bool -> Board -> Board
setBoardPiece coordinate withPiece board =
    Dict.update coordinate ( updateBoardCirclePiece withPiece ) board


setBoardOrigin : Board -> Board
setBoardOrigin board =
    let
        board' = Dict.map ( setBoardCirclePiece True ) board
    in
        setBoardPiece (4,4) False board'


-- We don't use BoardCircle here but it is needed 'cause
-- of Dict.filter : (comparable -> v -> Bool)
--                -> Dict comparable v
--                -> Dict comparable v
isBoardCircleAtRow : Row -> Coordinate -> BoardCircle -> Bool
isBoardCircleAtRow row coordinate boardCircle =
    if getX coordinate == row then
        True

    else
        False


getCirclesAtRow : Row -> Board -> BoardRow
getCirclesAtRow row board =
    Dict.filter ( isBoardCircleAtRow row ) board

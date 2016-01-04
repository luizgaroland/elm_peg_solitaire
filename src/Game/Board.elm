module Game.Board where

import Dict exposing (..)

type alias Coordinate = (Int, Int)


type alias Row = Int


type alias BoardCircle =
    {
        hasPiece : Bool
    }


type alias Board = Dict Coordinate BoardCircle


type alias BoardRow = Dict Coordinate BoardCircle


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


getCoordinate : Int -> Int -> Coordinate
getCoordinate x y =
    (x, y)


getCoordinatesRow : Int -> List Coordinate
getCoordinatesRow digit =
    List.map (getCoordinate digit) [1..7]


getCoordinatesList : List Coordinate
getCoordinatesList =
    List.concat <| List.map getCoordinatesRow [1..7]


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
    List.filter validateCoord getCoordinatesList


getBoardCircles : List BoardCircle
getBoardCircles =
    let
        fromDigitToCircle digit =
            boardCircle False

    in
        List.map fromDigitToCircle [1..33]


zipCoordWithBoardCircles : List Coordinate 
                         -> List BoardCircle 
                         -> List ( Coordinate, BoardCircle )
zipCoordWithBoardCircles coords circles =
    List.map2 (,) coords circles


createBoard : Board
createBoard =
    Dict.fromList <| zipCoordWithBoardCircles 
    getValidCoordinates getBoardCircles


updateCirclePiece : Bool -> Maybe BoardCircle -> Maybe BoardCircle
updateCirclePiece withPiece boardCircle =
    case boardCircle of
        Just boardCircle ->
            Just { boardCircle | hasPiece = withPiece }

        Nothing ->
            boardCircle


setBoardPiece : Bool -> Board -> Coordinate -> Board
setBoardPiece withPiece board coordinate  =
    Dict.update coordinate ( updateCirclePiece withPiece ) board


setCirclePiece : Bool -> Coordinate -> BoardCircle -> BoardCircle
setCirclePiece withPiece coordinate boardCircle =
    { boardCircle | hasPiece = withPiece }


setBoardOrigin : Board -> Board
setBoardOrigin board =
    let
        board' = Dict.map ( setCirclePiece True ) board
    in
        setBoardPiece False board' (4,4)


-- We don't use BoardCircle here but it is needed 'cause 
-- of Dict.filter : (comparable -> v -> Bool) 
--                -> Dict comparable v 
--                -> Dict comparable v
isCircleAtRow : Row -> Coordinate -> BoardCircle -> Bool
isCircleAtRow row coordinate boardCircle =
    if getX coordinate == row then
        True

    else
        False


getCirclesAtRow : Row -> Board -> BoardRow
getCirclesAtRow row board =
    Dict.filter ( isCircleAtRow row ) board

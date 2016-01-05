module Game.Board where

import Dict exposing (..)

type alias Coordinate = (Int, Int)


type Direction = Horizontal | Vertical


type alias Row = Int


type alias BoardCircle =
    {
        hasPiece : Bool
    }


type alias Board = Dict Coordinate BoardCircle


type alias BoardRow = Dict Coordinate BoardCircle


type alias BoardPosition = Dict Coordinate BoardCircle


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


circleAtCoordinate : Coordinate -> Coordinate -> BoardCircle -> Bool
circleAtCoordinate coordinate coordinate' boardCircle =
    if coordinate == coordinate' then
        True

    else
        False


getSurroundingCoordinates : Coordinate -> List Coordinate
getSurroundingCoordinates coordinate =
    let
        north = (,) (fst coordinate) <| ((snd coordinate) + 1)

        south = (,) (fst coordinate) <| ((snd coordinate) - 1)

        east = (,) ((fst coordinate) + 1) <| snd coordinate

        west = (,) ((fst coordinate) - 1) <| snd coordinate
    in
        [ north, south, east, west ]


isCircleOnListAndWithPiece : List Coordinate
                           -> Coordinate -> BoardCircle -> Bool
isCircleOnListAndWithPiece coordinates coordinate boardCircle =
    if List.member coordinate coordinates then
        if boardCircle.hasPiece then
            True
        else
            False
    else
        False

isCircleOnCoordinateWithPiece : Coordinate -> Coordinate -> BoardCircle -> Bool
isCircleOnCoordinateWithPiece coordinate coordinate' boardCircle =
    if coordinate == coordinate' then
        if boardCircle.hasPiece then
            True

        else
            False
    else
        False


isPieceSurrounded : Coordinate -> Board -> Bool
isPieceSurrounded coordinate board =
    let

        atCoordinateHavePiece = Dict.filter 
            ( isCircleOnCoordinateWithPiece coordinate ) board

        surroundingCoordinates = getSurroundingCoordinates coordinate

        surroundingCoordinates' =
            List.filter validateCoord surroundingCoordinates

        positions = Dict.filter 
            ( isCircleOnListAndWithPiece surroundingCoordinates' ) board

        positions' = Dict.toList positions

        atCoordinateHavePiece' = Dict.toList atCoordinateHavePiece

    in
        if List.length atCoordinateHavePiece' == 1 then
            if List.length positions' == List.length surroundingCoordinates' then
                True 

            else
                False
        else
            False
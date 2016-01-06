module Game.Board where

import Dict exposing (..)

type alias Coordinate = (Int, Int)


type Direction = North | South | East | West


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
    if (getX coord > 2 && getX coord < 6) 
    && (getY coord > 0 && getY coord < 4) then
        True

    else if (getX coord > 0 && getX coord < 8) 
    && (getY coord > 2 && getY coord < 6) then 
        True

    else if (getX coord > 2 && getX coord < 6)
    && (getY coord > 5 && getY coord < 8) then
        True

    else
        False


getCoordinate : Int -> Int -> Coordinate
getCoordinate x y =
    ( x, y )


getCoordinatesRow : Int -> List Coordinate
getCoordinatesRow digit =
    List.map ( getCoordinate digit ) [1..7]


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


setBoardPiece : Coordinate -> Bool -> Board -> Board
setBoardPiece coordinate withPiece board =
    Dict.update coordinate ( updateCirclePiece withPiece ) board


setCirclePiece : Bool -> Coordinate -> BoardCircle -> BoardCircle
setCirclePiece withPiece coordinate boardCircle =
    { boardCircle | hasPiece = withPiece }


setBoardOrigin : Board -> Board
setBoardOrigin board =
    let
        board' = Dict.map ( setCirclePiece True ) board
    in
        setBoardPiece (4,4) False board' 


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


isCircleOnListAndWithoutPiece : List Coordinate
                              -> Coordinate -> BoardCircle -> Bool
isCircleOnListAndWithoutPiece coordinates coordinate boardCircle =
    if List.member coordinate coordinates then
        if not boardCircle.hasPiece then
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


makeDirectionsFromBooleans : Bool -> Bool -> Bool -> Bool -> List Direction
makeDirectionsFromBooleans north south east west =
        (if north then North :: [] else [])
        ++ (if south then South :: [] else [])
        ++ (if east then East :: [] else [])
        ++ (if west then West :: [] else [])


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


canPlayHappenWhichDirection : Coordinate -> Board -> (Bool, List Direction)

canPlayHappenWhichDirection coordinate board =
    let
        atCoordinateHavePiece = 
            not
            <| Dict.isEmpty
            <| Dict.filter ( isCircleOnCoordinateWithPiece coordinate ) board

        north = addCoordinateOnColumn coordinate -1

        south = addCoordinateOnColumn coordinate 1

        east = addCoordinateOnRow coordinate 1

        west = addCoordinateOnRow coordinate -1

        furtherNorth = addCoordinateOnColumn coordinate -2

        furtherSouth = addCoordinateOnColumn coordinate 2

        furtherEast = addCoordinateOnRow coordinate 2

        furtherWest = addCoordinateOnRow coordinate -2

        northWithPiece =
            not 
            <| Dict.isEmpty 
            <| Dict.filter (isCircleOnListAndWithPiece [north]) board

        furtherNorthWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithoutPiece [furtherNorth]) board

        southWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithPiece [south]) board

        furtherSouthWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithoutPiece [furtherSouth]) board

        eastWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithPiece [east]) board

        furtherEastWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithoutPiece [furtherEast]) board

        westWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithPiece [west]) board

        furtherWestWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter (isCircleOnListAndWithoutPiece [furtherWest]) board

        northCan = northWithPiece && furtherNorthWithoutPiece

        southCan = southWithPiece && furtherSouthWithoutPiece

        eastCan = eastWithPiece && furtherEastWithoutPiece

        westCan = westWithPiece && furtherWestWithoutPiece

        directionList = 
            makeDirectionsFromBooleans northCan southCan eastCan westCan

        in
            if atCoordinateHavePiece then
                if northCan 
                || southCan 
                || eastCan 
                || westCan then
                
                ( True, directionList)

                else
                    ( False, [] )

            else
                ( False, [] )


makePlay : Coordinate -> Direction -> Board -> Board
makePlay coordinate direction board =
    let
        playAndDirection = canPlayHappenWhichDirection coordinate board

        doMakePlay = fst playAndDirection

        directions = snd playAndDirection

        validDirection = List.member direction directions

    in
        if doMakePlay && validDirection then
            case direction of
                North->
                    let
                        north = addCoordinateOnColumn coordinate -1

                        furtherNorth = addCoordinateOnColumn coordinate -2

                    in
                        setBoardPiece coordinate False
                        <| setBoardPiece furtherNorth True 
                        <| setBoardPiece north False board

                South->
                    let
                        south = addCoordinateOnColumn coordinate 1

                        furtherSouth = addCoordinateOnColumn coordinate 2

                    in
                        setBoardPiece coordinate False
                        <| setBoardPiece furtherSouth True 
                        <| setBoardPiece south False board

                East->
                    let
                        east = addCoordinateOnRow coordinate 1

                        furtherEast = addCoordinateOnRow coordinate 2

                    in
                        setBoardPiece coordinate False
                        <| setBoardPiece furtherEast True 
                        <| setBoardPiece east False board

                West->
                    let
                        west = addCoordinateOnRow coordinate -1

                        furtherWest = addCoordinateOnRow coordinate -2

                    in
                        setBoardPiece coordinate False
                        <| setBoardPiece furtherWest True 
                        <| setBoardPiece west False board
        else
            board
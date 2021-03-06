module Game.Logic where


import Dict exposing (..)
import Game.Definition exposing (..)
import Game.Board exposing (..)
import Game.BoardCircle exposing (..)


getInitialGame : Game
getInitialGame =
    {
        gameState = Origin
    ,   board = setBoardOrigin <| createBoard
    ,   cursor = cursorOrigin
    }


makeDirectionsFromBooleans : Bool -> Bool -> Bool -> Bool -> List Direction
makeDirectionsFromBooleans north south east west =
        (if north then North :: [] else [])
        ++ (if south then South :: [] else [])
        ++ (if east then East :: [] else [])
        ++ (if west then West :: [] else [])


canPlayHappenWhichDirection : Coordinate -> Board -> (Bool, List Direction)
canPlayHappenWhichDirection coordinate board =
    let
        atCoordinateHavePiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            ( isBoardCircleOnCoordinateWithPiece coordinate ) board

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
            <| Dict.filter
            (isBoardCircleOnListAndWithPiece [north]) board

        furtherNorthWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithoutPiece [furtherNorth]) board

        southWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithPiece [south]) board

        furtherSouthWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithoutPiece [furtherSouth]) board

        eastWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithPiece [east]) board

        furtherEastWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithoutPiece [furtherEast]) board

        westWithPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithPiece [west]) board

        furtherWestWithoutPiece =
            not
            <| Dict.isEmpty
            <| Dict.filter
            (isBoardCircleOnListAndWithoutPiece [furtherWest]) board

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

                ( True, directionList )

                else
                    ( False, directionList )

            else
                ( False, directionList )


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

                InvalidDirection->
                    board
        else
            board


isPlayPossible : Board -> Coordinate -> BoardCircle -> Bool
isPlayPossible board coordinate boardCircle =
    fst <| canPlayHappenWhichDirection coordinate board


arePlaysPossible : Board -> Bool
arePlaysPossible board =
    let
        listOfPossiblePlays = 
            Dict.toList 
            <| Dict.filter (isPlayPossible board) board

        amountOfPlays = List.length listOfPossiblePlays
    in
        if amountOfPlays > 0 then
            True
        else
            False


filterWithPiece : Coordinate -> BoardCircle -> Bool
filterWithPiece coordinate boardCircle =
    if boardCircle.hasPiece then
        True
    
    else
        False


doesTheBoardHaveOnePiece : Board -> Bool
doesTheBoardHaveOnePiece board =
    let
        numberOfPieces =
            List.length 
            <| Dict.toList
            <| Dict.filter filterWithPiece board
    
    in
        if numberOfPieces == 1 then
            True
        
        else
            False


getCursorAdjacentPlayCoordinates : Cursor -> Board -> List Coordinate
getCursorAdjacentPlayCoordinates cursor board =
    let
        playAndDirections = canPlayHappenWhichDirection cursor board
        
        directionsList = snd playAndDirections 
        
        north = addCoordinateOnColumn cursor -1

        south = addCoordinateOnColumn cursor 1

        east = addCoordinateOnRow cursor 1

        west = addCoordinateOnRow cursor -1
        
        northIs = List.member North directionsList
        
        southIs = List.member South directionsList
        
        eastIs = List.member East directionsList
        
        westIs = List.member West directionsList
        
        coordinatesList = []

    in
        (if northIs then north :: [] else [])
        ++ (if southIs then south :: [] else [])
        ++ (if eastIs then east :: [] else [])
        ++ (if westIs then west :: [] else [])

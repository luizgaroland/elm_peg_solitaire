module Game.Logic where

import Dict exposing (..)
import Game.Board exposing (..)
import Game.BoardCircle exposing (..)


type Direction = North | South | East | West


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

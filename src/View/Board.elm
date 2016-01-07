module View.Board where

import Html exposing (..)
import Html.Attributes  exposing (..)
import Dict exposing (..)


import View.BoardPeripherals exposing (..)
import Game.Board exposing (..)
import Game.BoardCircle exposing (..)
import Controls.Controls exposing (..)


type BoardCircleViewCase = WithPiece | WithoutPiece


isCoordinateAtCursor : Coordinate -> Cursor -> Bool
isCoordinateAtCursor coordinate cursor  =
    let
        cursorX = fst cursor
        cursorY = snd cursor

        coordinateX = fst coordinate
        coordinateY = snd coordinate
    in
        if cursorX == coordinateX && cursorY == coordinateY then
            True

        else
            False


renderBoardCircle : BoardCircleViewCase -> Coordinate -> Cursor -> Html
renderBoardCircle circleCase coordinate cursor =
    let
        isAtCursor = isCoordinateAtCursor coordinate cursor
    in    
        case circleCase of
            WithPiece ->
                if isAtCursor then
                    td []
                    [
                        span 
                        [ 
                            class circleWrapperClass
                        ,   title <| toString coordinate
                        ]
                            [
                                renderOuterCircleAtCursor
                            ,   piece
                            ]
                    ]

                else
                    td []
                    [
                        span 
                        [ 
                            class circleWrapperClass
                        ,   title <| toString coordinate
                        ]
                            [
                                renderOuterCircle
                            ,   piece
                            ]
                    ]

            WithoutPiece -> 
                if isAtCursor then
                    td []
                    [
                        span 
                        [ 
                            class circleWrapperClass
                        ,   title <| toString coordinate
                        ]
                            [
                                renderOuterCircleAtCursor
                            ]
                    ]

                else
                    td []
                    [
                        span 
                        [ 
                            class circleWrapperClass
                        ,   title <| toString coordinate
                        ]
                            [
                                renderOuterCircle
                            ]
                    ]

renderFillerCircle : Html
renderFillerCircle =
    td []
    [
        span [ class circleWrapperClass ]
            [
                fillerCircle
            ]
    ]


getHtmlCircleAndWrapper : Cursor -> (Coordinate, BoardCircle)  -> Html
getHtmlCircleAndWrapper cursor coordCircle =
    if (snd coordCircle).hasPiece then
        renderBoardCircle WithPiece (fst coordCircle) cursor
    else
        renderBoardCircle WithoutPiece (fst coordCircle) cursor


getHtmlFromBoardRow : Cursor -> BoardRow -> List Html
getHtmlFromBoardRow cursor boardRow =
    List.map (getHtmlCircleAndWrapper cursor) <| Dict.toList boardRow


renderTrimmedRow : Cursor -> BoardRow -> Html
renderTrimmedRow cursor row =
    let
        filler = 
            [ 
                renderFillerCircle
            ,   renderFillerCircle
            ]

        filler' =
            [ 
                renderFillerCircle
            ,   renderFillerCircle
            ]

        html = List.append filler (getHtmlFromBoardRow cursor row)

        html' = List.append html filler'

    in
        tr [] html'


renderRow : Cursor -> BoardRow -> Html
renderRow cursor row =
    tr [] <| getHtmlFromBoardRow cursor row


renderBoardTrimmedRowsTop : Board -> Cursor -> List Html
renderBoardTrimmedRowsTop board cursor =
    [
        renderTrimmedRow cursor <| getCirclesAtRow 1 board
    ,   renderTrimmedRow cursor <| getCirclesAtRow 2 board
    ]


renderBoardTrimmedRowsBot : Board -> Cursor -> List Html
renderBoardTrimmedRowsBot board cursor =
    [
        renderTrimmedRow cursor <| getCirclesAtRow 6 board
    ,   renderTrimmedRow cursor <| getCirclesAtRow 7 board
    ]


renderBoardCentralCluster : Board -> Cursor -> List Html
renderBoardCentralCluster board cursor =
    [
        renderRow cursor <| getCirclesAtRow 3 board
    ,   renderRow cursor <| getCirclesAtRow 4 board
    ,   renderRow cursor <| getCirclesAtRow 5 board
    ]


renderBoard : Board -> Cursor -> Html
renderBoard board cursor =
    let
        rowsTop =  renderBoardTrimmedRowsTop board cursor

        rowBot = renderBoardTrimmedRowsBot board cursor

        centralCluster = renderBoardCentralCluster board cursor
    in
        table [] <| rowsTop ++ centralCluster ++ rowBot

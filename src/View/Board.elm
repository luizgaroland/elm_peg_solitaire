module View.Board where

import Html exposing (..)
import Html.Attributes  exposing (..)
import Dict exposing (..)


import View.BoardPeripherals exposing (..)
import Game.Board exposing (..)


type BoardCircleViewCase = WithPiece | WithoutPiece | Filler


renderBoardCircle : BoardCircleViewCase -> Html
renderBoardCircle circleCase =
    case circleCase of
        WithPiece ->
            td []
            [
                span [ class circleWrapperClass ]
                    [
                        renderOuterCircle
                    ,   piece
                    ]
            ]

        WithoutPiece -> 
            td []
            [
                span [ class circleWrapperClass ]
                    [
                        renderOuterCircle
                    ]
            ]

        Filler ->
            td []
            [
                span [ class circleWrapperClass ]
                    [
                        fillerCircle
                    ]
            ]


getHtmlCircleAndWrapper : (Coordinate, BoardCircle) -> Html
getHtmlCircleAndWrapper coordCircle =
    if (snd coordCircle).hasPiece then
        renderBoardCircle WithPiece
    else
        renderBoardCircle WithoutPiece


getHtmlFromBoardRow : BoardRow -> List Html
getHtmlFromBoardRow boardRow =
    List.map getHtmlCircleAndWrapper <| Dict.toList boardRow


renderTrimmedRow : BoardRow -> Html
renderTrimmedRow row =
    let
        filler = [ renderBoardCircle Filler
                 , renderBoardCircle Filler
                 ]

        filler' = [ renderBoardCircle Filler
                  , renderBoardCircle Filler
                  ]

        html = List.append filler ( getHtmlFromBoardRow row )

        html' = List.append html filler'

    in
        tr [] html'


renderRow : BoardRow -> Html
renderRow row =
    tr [] <| getHtmlFromBoardRow row


renderBoardTrimmedRowsTop : Board -> List Html
renderBoardTrimmedRowsTop board =
    [
        renderTrimmedRow <| getCirclesAtRow 1 board
    ,   renderTrimmedRow <| getCirclesAtRow 2 board
    ]


renderBoardTrimmedRowsBot : Board -> List Html
renderBoardTrimmedRowsBot board =
    [
        renderTrimmedRow <| getCirclesAtRow 6 board
    ,   renderTrimmedRow <| getCirclesAtRow 7 board
    ]


renderBoardCentralCluster : Board -> List Html
renderBoardCentralCluster board =
    [
        renderRow <| getCirclesAtRow 3 board
    ,   renderRow <| getCirclesAtRow 4 board
    ,   renderRow <| getCirclesAtRow 5 board
    ]


renderBoard : Board -> Html
renderBoard board =
    let
        rowsTop =  renderBoardTrimmedRowsTop board

        rowBot = renderBoardTrimmedRowsBot board

        centralCluster = renderBoardCentralCluster board
    in
        table [] <| rowsTop ++ centralCluster ++ rowBot

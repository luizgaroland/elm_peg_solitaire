module View.Board where

import Html exposing (..)
import Html.Attributes  exposing (..)


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


--getHtmlFromBoardRow : BoardRow -> List Html
--getHtmlFromBoardRow boardRow =
--    List.Map



renderTrimmedRow : Html
renderTrimmedRow =
    tr []
    [
        renderBoardCircle Filler
    ,   renderBoardCircle Filler
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle Filler
    ,   renderBoardCircle Filler
    ]


renderRow : Bool -> Html
renderRow cpiece =
    tr []
    [
        renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle ( if cpiece then WithPiece else WithoutPiece )
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ,   renderBoardCircle WithPiece
    ]


renderBoardTrimmedRow : List Html
renderBoardTrimmedRow =
    [
        renderTrimmedRow
    ,   renderTrimmedRow
    ]


renderBoardCentralCluster : List Html
renderBoardCentralCluster =
    [
        renderRow True
    ,   renderRow False
    ,   renderRow True
    ]


renderBoard : Html
renderBoard =
    table [] <| 
        renderBoardTrimmedRow ++ renderBoardCentralCluster ++
        renderBoardTrimmedRow

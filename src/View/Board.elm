module View.Board where

import Html exposing (..)
import Html.Attributes  exposing (..)
import View.BoardPeripherals exposing (..)


type BoardCircleCase = WithPiece | WithoutPiece | Filler


renderBoardCircle : BoardCircleCase -> Html
renderBoardCircle circleCase =
    case circleCase of
        WithPiece ->
            span [ class circleWrapperClass ]
                [
                    boardCircle
                ,   piece
                ]

        WithoutPiece -> 
            span [ class circleWrapperClass ]
                [
                    boardCircle 
                ]

        Filler ->
            span [ class circleWrapperClass ]
                [
                    fillerCircle
                ]


renderTrimmedRow : Html
renderTrimmedRow =
    tr []
    [
        td []
        [
            renderBoardCircle Filler
        ]
    ,   td []
        [
            renderBoardCircle Filler
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle Filler
        ]
    ,   td []
        [
            renderBoardCircle Filler
        ]
    ]


renderRow : Bool -> Html
renderRow cpiece =
    tr []
    [
        td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle <| if cpiece then WithPiece else WithoutPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
    ,   td []
        [
            renderBoardCircle WithPiece
        ]
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

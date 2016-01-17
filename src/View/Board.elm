module View.Board where


import Html exposing (..)
import Html.Attributes  exposing (..)
import Dict exposing (..)


import View.BoardPeripherals exposing (..)
import Game.Definition exposing (..)
import Game.Board exposing (..)
import Game.Logic exposing (..)


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


renderBoardCircle : BoardCircleViewCase -> Coordinate -> Game -> Html
renderBoardCircle circleCase coordinate game =
    let
        cursor = game.cursor
        
        state = game.gameState
        
        isAtCursor = isCoordinateAtCursor coordinate cursor
        
        adjacentCoord = getCursorAdjacentPlayCoordinates game.cursor game.board
        
        isCoordinateAdjacent = List.member coordinate adjacentCoord
        
    in
        if state == Playing
        || state == Origin then
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
                        
        else if state == PlayingToChooseDirection then
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
                                    renderOuterCircleAtCursorChoosing
                                ,   piece
                                ]
                        ]
                    
                    else if isCoordinateAdjacent then
                        td []
                        [
                            span 
                            [ 
                                class circleWrapperClass
                            ,   title <| toString coordinate
                            ]
                                [
                                    renderOutCircleDirectionToChoose
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
                                    renderOuterCircleAtCursorChoosing
                                ]
                        ]
    
                    else if isCoordinateAdjacent then
                        td []
                        [
                            span 
                            [ 
                                class circleWrapperClass
                            ,   title <| toString coordinate
                            ]
                                [
                                    renderOutCircleDirectionToChoose
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
        else
            td [] []


renderFillerCircle : Html
renderFillerCircle =
    td []
    [
        span [ class circleWrapperClass ]
            [
                fillerCircle
            ]
    ]


getHtmlCircleAndWrapper : Game -> (Coordinate, BoardCircle)  -> Html
getHtmlCircleAndWrapper game coordCircle =
    if (snd coordCircle).hasPiece then
        renderBoardCircle WithPiece (fst coordCircle) game

    else
        renderBoardCircle WithoutPiece (fst coordCircle) game


getHtmlFromBoardRow : Game -> BoardRow -> List Html
getHtmlFromBoardRow game boardRow =
    List.map (getHtmlCircleAndWrapper game) <| Dict.toList boardRow


renderTrimmedRow : Game -> BoardRow -> Html
renderTrimmedRow game row =
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

        html = List.append filler (getHtmlFromBoardRow game row)

        html' = List.append html filler'

    in
        tr [] html'


renderRow : Game -> BoardRow -> Html
renderRow game row =
    tr [] <| getHtmlFromBoardRow game row


renderBoardTrimmedRowsTop : Game -> List Html
renderBoardTrimmedRowsTop game =
    [
        renderTrimmedRow game <| getCirclesAtRow 1 game.board
    ,   renderTrimmedRow game <| getCirclesAtRow 2 game.board
    ]


renderBoardTrimmedRowsBot : Game -> List Html
renderBoardTrimmedRowsBot game =
    [
        renderTrimmedRow game <| getCirclesAtRow 6 game.board
    ,   renderTrimmedRow game <| getCirclesAtRow 7 game.board
    ]


renderBoardCentralCluster : Game -> List Html
renderBoardCentralCluster game =
    [
        renderRow game <| getCirclesAtRow 3 game.board
    ,   renderRow game <| getCirclesAtRow 4 game.board
    ,   renderRow game <| getCirclesAtRow 5 game.board
    ]


renderBoard : Game -> Html
renderBoard game =
    let
        rowsTop =  renderBoardTrimmedRowsTop game

        rowBot = renderBoardTrimmedRowsBot game

        centralCluster = renderBoardCentralCluster game

    in
        table [] <| rowsTop ++ centralCluster ++ rowBot

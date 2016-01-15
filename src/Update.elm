module Update where


import Game.Definition exposing (..)
import Game.Logic exposing (..)
import Game.Controls exposing (..)
import Game.BoardCircle exposing (..)


updateCursor : GameState -> Cursor -> Cursor -> Cursor
updateCursor state cursor deviation =
    let
        oldCursor =
            (,)
            (fst cursor)
            (snd cursor)

        newCursor =
            (,)
            ((fst oldCursor) + (fst deviation))
            ((snd oldCursor) + (snd deviation))

    in
        if state == PlayingToChooseDirection then
            oldCursor
        else
            if validateCoord newCursor
            then
                newCursor
    
            else
                oldCursor


updateGame : Play -> Game -> Game
updateGame play game =
    let
        makePlayPressed = play.makePlayPressed

        newCursor = updateCursor game.gameState game.cursor play.cursorDeviation
        
        cursorDirection = play.cursorDirection
        
        board = game.board

        gameState = game.gameState

        newGame = { game | cursor = newCursor }

        playAndDirection = 
            canPlayHappenWhichDirection newGame.cursor newGame.board

        canPlayHappen = fst playAndDirection

        shouldPlayHappen = canPlayHappen && makePlayPressed

        playDirections = snd playAndDirection

        maybeFirstDirection = List.head playDirections

    in
        case gameState of
            Origin ->
                if shouldPlayHappen then
                    if List.length playDirections == 1 then
                        case maybeFirstDirection of
                            Just direction ->
                                { newGame |
                                    board =
                                        makePlay newGame.cursor direction
                                        newGame.board
                                ,   gameState = Playing
                                }

                            Nothing->
                                newGame
                    
                    else
                        newGame

                else
                    newGame

            Playing ->
                if shouldPlayHappen then
                    if List.length playDirections == 1 then
                        case maybeFirstDirection of
                            Just direction ->
                                { newGame |
                                    board =
                                        makePlay newGame.cursor direction
                                        newGame.board
                                ,   gameState = Playing
                                }

                            Nothing->
                                newGame

                    else if List.length playDirections > 1 then
                        { newGame |
                            gameState = PlayingToChooseDirection
                        }

                    else
                        newGame
                        
                else
                    newGame

            PlayingToChooseDirection ->
                if canPlayHappen && List.member cursorDirection playDirections then
                    { newGame |
                        board =
                            makePlay newGame.cursor cursorDirection 
                            newGame.board
                    ,   gameState = Playing
                    }
                    
                else
                    newGame

            Loss ->
                newGame

            Win ->
                newGame


fromSignalsToPlayRecord : Bool -> Cursor -> Direction -> Play
fromSignalsToPlayRecord makePlayPressed deviation direction =
    {
        makePlayPressed = makePlayPressed
    ,   cursorDirection = direction
    ,   cursorDeviation = deviation
    }


getPlay : Signal Play
getPlay =
    Signal.map3 
        fromSignalsToPlayRecord
        makePlayKeyPressed getCursorDeviation getCursorDirection


getGame : Signal Game
getGame =
    Signal.foldp updateGame getInitialGame getPlay
        

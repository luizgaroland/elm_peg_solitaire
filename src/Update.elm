module Update where


import Game.Definition exposing (..)
import Game.Logic exposing (..)
import Game.Controls exposing (..)
import Game.BoardCircle exposing (..)
import Game.Board exposing (..)


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
        if state == Playing
        || state == Origin then
            if validateCoord newCursor
            then
                newCursor
    
            else
                oldCursor

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
        if arePlaysPossible board then
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
                    if canPlayHappen
                    && List.member cursorDirection playDirections then
                        { newGame |
                            board =
                                makePlay newGame.cursor cursorDirection 
                                newGame.board
                        ,   gameState = Playing
                        }
                        
                    else
                        newGame
    
                Loss ->
                    if makePlayPressed then
                        getInitialGame
                        
                    else
                        newGame
    
                Win ->
                    if makePlayPressed then
                        getInitialGame
                        
                    else
                        newGame
                    
        else if doesTheBoardHaveOnePiece board then
            if makePlayPressed then
                getInitialGame
                        
            else
                { 
                    newGame |
                        gameState = Win
                }
        
        else
            if makePlayPressed then
                getInitialGame
                        
            else
                { 
                    newGame |
                        gameState = Loss
                }


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
        

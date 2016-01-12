module Update where


import Game.Definition exposing (..)
import Game.Logic exposing (..)
import Game.Controls exposing (..)


updateGame : Game -> Cursor -> Bool -> Game
updateGame game newCursor makePlayPressed =
    let
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

                    else
                        newGame

                else
                    newGame

            PlayingToChooseDirection ->
                newGame

            Loss ->
                newGame

            Win ->
                newGame


processPlay : (Bool, Cursor) -> Game -> Game
processPlay play game =
    let
        makePlay = fst play

        cursor = snd play
    in
        updateGame game cursor makePlay


getGame : Signal Game
getGame =
    Signal.foldp processPlay getInitialGame getPlay
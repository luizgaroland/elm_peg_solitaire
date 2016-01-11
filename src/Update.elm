module Update where


import Game.Definition exposing (..)
import Game.Logic exposing (..)
import Game.Controls exposing (..)


updateGame : Game -> Cursor -> Game
updateGame game newCursor =
    let
        board = game.board

        gameState = game.gameState

        newGame = { game | cursor = newCursor }

    in
        case gameState of
            Origin ->
                newGame

            Playing ->
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
        updateGame game cursor


getGame : Signal Game
getGame =
    Signal.foldp processPlay getInitialGame getPlay
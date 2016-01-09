module Update where

import Game.Logic exposing (..)
import Controls.Controls exposing (..)


updateGame : Game -> Cursor -> Direction -> Bool -> Game
updateGame game cursor direction makePlay =
    let
        board = game.board

        gameState = game.gameState

    in
        case gameState of
            Origin ->
                game

            Playing ->
                game

            PlayingToChooseDirection ->
                game

            Loss ->
                game

            Win ->
                game


getGame : Signal Game
getGame =
    Signal.foldp (updateGame
module View.Canvas where


import Html exposing (..)
import Html.Attributes  exposing (..)


import Game.Logic exposing (..)
import View.Board exposing (..)


renderGame : Game -> Html
renderGame game =
    let
        state = game.gameState
        board = game.board
    in

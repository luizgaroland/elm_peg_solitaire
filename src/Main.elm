module Main(main) where

import Mouse
--import Keyboard
import Html exposing (..)
import Graphics.Element exposing (..)


import Game.Logic exposing (..)
import View.Canvas exposing (..)
--import Game.BoardCircle exposing (..)
import Controls.Controls exposing (..)


boardSignal : Signal Board
boardSignal =
    let
        board = setBoardOrigin <| createBoard
    in
        Signal.foldp (\clk board -> makePlay (4,2) East board) board Mouse.clicks 


main: Signal Html
main =
    Signal.foldp  renderGame

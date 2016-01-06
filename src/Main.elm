module Main(main) where

import Html exposing (..)
import Graphics.Element exposing (..)

import Game.Board exposing (..)
import View.Board exposing (..)

main: Html
main =
    renderBoard <| makePlay (2,3) South <| makePlay (4,2) East <| setBoardOrigin <| createBoard


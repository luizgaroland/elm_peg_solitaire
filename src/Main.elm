module Main(main) where

--import Html exposing (..)
import Graphics.Element exposing (..)

import Game.Board exposing (..)
import View.Board exposing (..)

main: Element
main =
    show <| isPieceSurrounded (4, 4) <| setBoardOrigin createBoard


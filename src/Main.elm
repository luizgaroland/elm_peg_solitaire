module Main(main) where

import Graphics.Element exposing (..)
import Game.Board exposing (..)


main: Element 
main = show <| getCirclesAtRow 1 <|setBoardOrigin createBoard


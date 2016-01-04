module Main(main) where

import Graphics.Element exposing (..)
import Game.Board exposing (..)


main: Element 
main = show <| List.length <| List.filter validateCoord <| List.map fromTupleToCoordinate getTuplesList

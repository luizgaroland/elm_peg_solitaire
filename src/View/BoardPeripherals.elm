module View.BoardPeripherals where

import Html exposing (..)
import Html.Attributes  exposing (..)

boardCircleClass : String
boardCircleClass = "fa fa-circle fa-stack-2x fa-inverse"


pieceClass : String
pieceClass = "fa fa-circle fa-stack-1x"


circleWrapperClass : String
circleWrapperClass = "fa-stack fa-lg"


boardCircleFillerClass : String
boardCircleFillerClass = "filler-circle " ++ boardCircleClass


renderOuterCircle : Html
renderOuterCircle = i [ class boardCircleClass ] []


piece : Html
piece = i [ class pieceClass ] []


fillerCircle : Html
fillerCircle = i [ class boardCircleFillerClass ] []
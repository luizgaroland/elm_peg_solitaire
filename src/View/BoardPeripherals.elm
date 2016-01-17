module View.BoardPeripherals where


import Html exposing (..)
import Html.Attributes  exposing (..)


boardCircleClass : String
boardCircleClass = "fa fa-circle fa-stack-2x fa-inverse"


boardCircleAtCursor : String
boardCircleAtCursor = "fa fa-circle fa-stack-2x at_cursor"


boardCircleAtCursorChoosing : String
boardCircleAtCursorChoosing = "fa fa-circle fa-stack-2x cursor_choosing"


boardCircleDirectionToChoose : String
boardCircleDirectionToChoose = "fa fa-circle fa-stack-2x direction"


pieceClass : String
pieceClass = "fa fa-circle fa-stack-1x"


circleWrapperClass : String
circleWrapperClass = "fa-stack fa-lg"


boardCircleFillerClass : String
boardCircleFillerClass = "filler-circle " ++ boardCircleClass


renderOuterCircle : Html
renderOuterCircle = i [ class boardCircleClass ] []


renderOuterCircleAtCursor : Html
renderOuterCircleAtCursor = i [ class boardCircleAtCursor ] []


renderOuterCircleAtCursorChoosing : Html
renderOuterCircleAtCursorChoosing = i [ class boardCircleAtCursorChoosing ] []


renderOutCircleDirectionToChoose : Html
renderOutCircleDirectionToChoose = i [ class boardCircleDirectionToChoose ] []


piece : Html
piece = i [ class pieceClass ] []


fillerCircle : Html
fillerCircle = i [ class boardCircleFillerClass ] []
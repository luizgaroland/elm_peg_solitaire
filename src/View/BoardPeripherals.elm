module View.BoardPeripherals where

boardCircleClass : String
boardCircleClass = "fa fa-circle fa-stack-2x fa-inverse"


pieceClass : String
pieceClass = "fa fa-circle fa-stack-1x"


circleWrapperClass : String
circleWrapperClass = "fa-stack fa-lg"


boardCircleFillerClass : String
boardCircleFillerClass = "filler-circle " ++ boardCircleClass


boardCircle : Html
boardCircle = i [ class boardCircleClass ] []


piece : Html
piece = i [ class pieceClass ] []


fillerCircle : Html
fillerCircle = i [ class boardCircleFillerClass ] []
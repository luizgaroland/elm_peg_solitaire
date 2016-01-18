module View.Canvas where


import Html exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)


import Update exposing (..)
import View.Board exposing (..)
import Game.Definition exposing (..)


type alias WindowDimension = (Int, Int)
    

mainFlowDown : WindowDimension -> Game -> Element
mainFlowDown (w, h) game =
    if game.gameState == Origin
    || game.gameState == Playing
    || game.gameState == PlayingToChooseDirection then
        flow down
            [
                container w 100 middle
                    <| Html.toElement 240 100
                    <| h1 [] [ Html.text "Peg Leg Solitaire" ]
             
            ,   container w 325 middle
                    <| Html.toElement 335 325
                    <| renderBoard game 

            ,   container w 80 middle
                    <| Html.toElement 90 50
                    <| h2 [] [ Html.text "Controls" ]
                    
            ,   container w 50 middle
                    <| Html.toElement 121 50
                    <| p []
                        [ 
                            Html.text 
                                "Space - Make Play" 
                        ]
                        
            ,   container w 50 middle
                    <| Html.toElement 303 50
                    <| p []
                        [ 
                            Html.text 
                                "Arrows - Move Cursor | Choose Play Direction" 
                        ]
            ]            
    
    else if game.gameState == Win then
        flow down
            [
                container w 100 middle
                    <| Html.toElement 240 100
                    <| h1 [] [ Html.text "Peg Leg Solitaire" ]
                    
            ,   container w 150 middle
                    <| Html.toElement 505 80
                    <| h1 [] [ 
                                Html.text 
                                    "Huzzah! You Win, Congratulations!" 
                             ]
                             
            ,   container w 150 middle
                    <| Html.toElement 185 80
                    <| h4 [] [ 
                                Html.text 
                                    "Press Space to Play Again!" 
                             ]
                             
            ,   container w 80 middle
                <| Html.toElement 90 50
                <| h2 [] [ Html.text "Controls" ]
                
            ,   container w 50 middle
                    <| Html.toElement 121 50
                    <| p []
                        [ 
                            Html.text 
                                "Space - Make Play" 
                        ]
                        
            ,   container w 50 middle
                    <| Html.toElement 303 50
                    <| p []
                        [ 
                            Html.text 
                                "Arrows - Move Cursor | Choose Play Direction" 
                        ]
            ]
    
    else if game.gameState == Loss then
        flow down
            [
                container w 100 middle
                    <| Html.toElement 240 100
                    <| h1 [] [ Html.text "Peg Leg Solitaire" ]
                    
            ,   container w 150 middle
                    <| Html.toElement 355 105
                    <| h1 [] [ Html.text "You lost! Sucks to be you!" ]
                    
            ,   container w 150 middle
                    <| Html.toElement 185 80
                    <| h4 [] [ 
                                Html.text 
                                    "Press Space to Play Again!" 
                             ]       

            ,   container w 80 middle
                <| Html.toElement 90 50
                <| h2 [] [ Html.text "Controls" ]
                
            ,   container w 50 middle
                    <| Html.toElement 121 50
                    <| p []
                        [ 
                            Html.text 
                                "Space - Make Play" 
                        ]
                        
            ,   container w 50 middle
                    <| Html.toElement 303 50
                    <| p []
                        [ 
                            Html.text 
                                "Arrows - Move Cursor | Choose Play Direction" 
                        ]
            ]

    else
        flow down
            [
                container w 100 middle
                    <| Html.toElement 240 100
                    <| h1 [] [ Html.text "Peg Leg Solitaire" ]
            ]


renderCanvas : Signal Element
renderCanvas =
    Signal.map2 mainFlowDown Window.dimensions getGame
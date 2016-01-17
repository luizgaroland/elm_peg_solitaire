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
            ]            
    
    else if game.gameState == Win then
        flow down
                [
                    container w 100 middle
                        <| Html.toElement 240 100
                        <| h1 [] [ Html.text "Peg Leg Solitaire" ]
                ,   container w 325 middle
                        <| Html.toElement 505 80
                        <| h1 [] [ 
                                    Html.text 
                                        "Huzzah! You Win, Congratulations!" 
                                 ]
                ]
    
    else if game.gameState == Loss then
        flow down
                [
                    container w 100 middle
                        <| Html.toElement 240 100
                        <| h1 [] [ Html.text "Peg Leg Solitaire" ]
                ,   container w 325 middle
                        <| Html.toElement 365 105
                        <| h1 [] [ Html.text "You lost! Sucks to be you!" ]
                        
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
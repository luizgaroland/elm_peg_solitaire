module View.Canvas where


import Html exposing (..)
import Graphics.Element exposing (..)
import Window exposing (..)
import Text exposing (..)


import Update exposing (..)
import View.Board exposing (..)
import Game.Definition exposing (..)


type alias WindowDimension = (Int, Int)
    

titleElement : Int -> List Element
titleElement w =
    [
        container w 100 middle
        <| centered 
        <| bold
        <| Text.height 40
        <| fromString "Peg Leg Solitaire"
    ]


gameElement : Int -> Game -> List Element
gameElement w game =
    [
        container w 326 middle
        <| Html.toElement 335 326
        <| renderBoard game 
    ]


controlFooterElement : Int -> List Element
controlFooterElement w =
    [
        container w 80 middle
        <| centered 
        <| bold
        <| Text.height 25
        <| fromString "Controls"
                
    ,   container w 50 middle
        <| centered 
        <| Text.height 15
        <| fromString "Space - Make Play"
            
    ,   container w 50 middle
        <| centered 
        <| Text.height 15
        <| fromString "Arrows - Move Cursor | Choose Play Direction"
    ]


winMessageElement : Int -> List Element
winMessageElement w =
    [
        container w 163 middle
        <| centered 
        <| Text.height 30
        <| fromString "Huzzah! You Win, Congratulations!"
                 
    ,   container w 163 middle
        <| centered 
        <| Text.height 20
        <| fromString "Press Space to Play Again!"
    ]


lossMessageElement : Int -> List Element
lossMessageElement w =
    [
        container w 163 middle
        <| centered 
        <| Text.height 30
        <| fromString "You lost! Sucks to be you!"
                    
    ,   container w 163 middle
        <| centered 
        <| Text.height 20
        <| fromString "Press Space to Play Again!"
    ] 


mainFlowDown : WindowDimension -> Game -> Element
mainFlowDown (w, h) game =
    if game.gameState == Origin
    || game.gameState == Playing
    || game.gameState == PlayingToChooseDirection then
        flow down
        <| (titleElement w) 
        ++ (gameElement w game) 
        ++ (controlFooterElement w)
    
    else if game.gameState == Win then
        flow down
        <| (titleElement w)
        ++ (winMessageElement w) 
        ++ (controlFooterElement w)
         
    else if game.gameState == Loss then
        flow down
        <| (titleElement w) 
        ++ (lossMessageElement w) 
        ++ (controlFooterElement w)

    else
        flow down []


renderCanvas : Signal Element
renderCanvas =
    Signal.map2 mainFlowDown Window.dimensions getGame
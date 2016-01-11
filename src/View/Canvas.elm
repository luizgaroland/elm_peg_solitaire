module View.Canvas where


import Html exposing (..)


import Update exposing (..)
import View.Board exposing (..)


renderGame : Signal Html
renderGame = 
    Signal.map (\game -> renderBoard game.board game.cursor) getGame

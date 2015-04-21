module Main where

import Window
import Keyboard
import Random
import Mouse

import View as V
import Model as M
import Controller as C
import Signal exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

main : Signal Element
main = V.render <~ Window.dimensions ~ (foldp C.stepGame M.defaultGame input)

-- Input

frameRate : Signal Time
frameRate = fps 30

obsInterval : Signal Time
obsInterval = every <| 500 * millisecond

input : Signal C.Event
input = mergeMany [
          map C.Tick keyboardInput ,
          map C.Add createObstacle,
          map (\_ -> C.Click) Mouse.clicks
        ]

createObstacle : Signal M.Obstacle
createObstacle = M.newObstacle <~ (map2 (\x y -> toFloat x / toFloat y) Window.width Window.height) ~ topOrBottom

topOrBottom : Signal M.Position
topOrBottom = let position f = if | f < 0.4 -> M.Bottom
                                   | f >= 0.4 && f < 0.6 -> M.Skip
                                   | f >= 0.6 -> M.Top
              in map position (map2 (\x y -> toFloat x / toFloat y) Window.width Window.height)

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in map isUp <| sampleOn frameRate Keyboard.arrows
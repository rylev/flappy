module Main where

import Window
import Keyboard
import Random
import Mouse

import View as V
import Model as M
import Controller as C

main : Signal Element
main = lift V.render Window.dimensions ~ (foldp C.stepGame M.defaultGame input)

-- Input

frameRate : Signal Time
frameRate = fps 30

obsInterval : Signal Time
obsInterval = every <| 500 * millisecond

input : Signal C.Event
input = merges [
          lift C.Tick keyboardInput,
          lift C.Add createObstacle,
          lift (\_ -> C.Click) Mouse.clicks
        ]

createObstacle : Signal M.Obstacle
createObstacle = M.newObstacle <~ Random.float obsInterval ~ topOrBottom

topOrBottom : Signal M.Position
topOrBottom = let position f = if | f < 0.4 -> M.Bottom
                                   | f >= 0.4 && f < 0.6 -> M.Skip
                                   | f >= 0.6 -> M.Top
              in lift position <| Random.float obsInterval

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in lift isUp <| sampleOn frameRate Keyboard.arrows
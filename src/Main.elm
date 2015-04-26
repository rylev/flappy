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
import Random

main : Signal Element
main = V.render <~ Window.dimensions ~ (foldp C.stepGame M.defaultGame input)

-- Input

frameRate : Signal Time
frameRate = fps 30

obsInterval : Signal Time
obsInterval = every <| 500 * millisecond

input : Signal C.Event
input = mergeMany [
          map C.Tick keyboardInput,
          map C.Add obstacles,
          map (\_ -> C.Click) Mouse.clicks
        ]

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in map isUp <| sampleOn frameRate Keyboard.arrows

toPosition : Float -> M.Position
toPosition f = if | f < 0.4 -> M.Bottom
                | f >= 0.4 && f < 0.6 -> M.Skip
                | f >= 0.6 -> M.Top

randomFloat : Signal Float
randomFloat = let randomSeed = map (\time -> Random.initialSeed (floor time)) <| Time.every Time.second
                  generator = Random.float 0 1
                  doGeneration seed = Random.generate generator seed
              in map (\(float, _) -> float) <| map doGeneration randomSeed


obstacles : Signal M.Obstacle
obstacles = map2 (\f1 f2 -> M.newObstacle f1 (toPosition f2)) randomFloat randomFloat
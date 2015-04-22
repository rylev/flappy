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
          map C.Tick keyboardInput ,
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

createObstacle : Random.Seed -> M.Obstacle
createObstacle seed = let (randomFloat, newSeed) = Random.generate obstacleGenerator seed
                          position = toPosition randomFloat
                      in M.newObstacle randomFloat position newSeed

obstacleGenerator : Random.Generator Float
obstacleGenerator = Random.float 0 1

obstacles : Signal M.Obstacle
obstacles = foldp (\_ prevObs -> createObstacle prevObs.seed) firstObstacle <| Time.every Time.second

-- TODO: Since the first obstacle is always the same. Create a Random first obstacle
firstObstacle : M.Obstacle
firstObstacle = M.newObstacle 9.2 M.Top <| Random.initialSeed 123
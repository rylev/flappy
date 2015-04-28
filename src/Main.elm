module Main where

import Window
import Keyboard
import Mouse
import Signal exposing (..)
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Random exposing (Seed)

import View
import Model
import Controller exposing (Event(Tick,Add,Click))

main : Signal Element
main = View.render <~ Window.dimensions ~ (foldp Controller.stepGame Model.defaultGame input)

-- Input

frameRate : Signal Time
frameRate = Time.fps 30

obsInterval : Signal Time
obsInterval = Time.every <| 500 * Time.millisecond

input : Signal Event
input = mergeMany [
          map Tick keyboardInput,
          map Add obstacles,
          map (\_ -> Click) Mouse.clicks
        ]

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in map isUp <| sampleOn frameRate Keyboard.arrows

toPosition : Float -> Model.Position
toPosition f = if | f < 0.45              -> Model.Bottom
                  | f >= 0.45 && f < 0.55 -> Model.Skip
                  | f >= 0.55             -> Model.Top

randomSeed : Signal Seed
randomSeed = map (\time -> Random.initialSeed (floor time)) <| Time.every Time.second

randomFloats : Signal (Float, Float)
randomFloats = let generator = Random.float 0 1
                   doGeneration seed = Random.generate generator seed
               in map (\(float, newSeed) -> (float,fst (doGeneration newSeed))) <| map doGeneration randomSeed

obstacles : Signal Model.Obstacle
obstacles = map (\(f1,f2) -> Model.newObstacle f1 (toPosition f2)) randomFloats
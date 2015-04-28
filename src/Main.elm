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

input : Signal Event
input = mergeMany [
          map Tick keyboardInput,
          map Add randomSeed,
          map (\_ -> Click) Mouse.clicks
        ]

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in map isUp <| sampleOn frameRate Keyboard.arrows

frameRate : Signal Time
frameRate = Time.fps 30

obsInterval : Signal Time
obsInterval = Time.every <| 500 * Time.millisecond

randomSeed : Signal Seed
randomSeed = map (\time -> Random.initialSeed (floor time)) <| obsInterval
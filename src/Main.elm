module Main exposing (main)

import Html.App as App
import Keyboard
import Mouse
import Element exposing (..)
import Time exposing (Time)
import Random exposing (Seed)
import AnimationFrame

import View
import Model
import Controller exposing (Event(Tick,Add,Click,UpArrowPressed,UpArrowReleased))

main = App.program
  { init = (Model.defaultGame, Cmd.none)
  , view = toHtml << (View.render (1270, 600))
  , update = \event game -> (Controller.stepGame event game, Cmd.none)
  , subscriptions = input
  }

-- Input


input : Model.Game -> Sub Event
input _ = Sub.batch [
          arrowPresses,
          arrowReleases,
          tick,
          Sub.map Add randomSeed,
          Mouse.clicks (\_ -> Click)
        ]

arrowPresses : Sub Event
arrowPresses = let toEvent keyCode = case keyCode of
                  38 -> UpArrowPressed
                  _  -> Tick
                in Keyboard.downs toEvent

arrowReleases : Sub Event
arrowReleases = let toEvent keyCode = case keyCode of
                  38 -> UpArrowReleased
                  _  -> Tick
                in Keyboard.ups toEvent

tick : Sub Event
tick = AnimationFrame.diffs (\_ -> Tick)

obsInterval : (Time -> a) -> Sub a
obsInterval = Time.every <| 500 * Time.millisecond

randomSeed : Sub Seed
randomSeed = obsInterval (Random.initialSeed << floor)
module Input exposing (input)

import AnimationFrame
import Keyboard
import Mouse
import Time exposing (Time)
import Random exposing (Seed)

import Controller exposing (Event(Tick,Add,Click,UpArrowPressed,UpArrowReleased))

input : Sub Event
input = Sub.batch [
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


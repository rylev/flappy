module View where

import Model exposing (Obstacle, Game, Bird, playArea, GameState(Active,GameOver))
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color

render : (Int, Int) -> Game -> Element
render winDim g = let renderScreen c = bg winDim <| renderPlayArea c
  in case g.state of
    Active -> renderScreen [renderBird g.bird, renderObs g.obstacles]
    GameOver -> renderScreen [toForm <| show "Game Over!!"]

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color Color.green <| container ww wh middle pa

renderPlayArea : List Form -> Element
renderPlayArea con = let area = collage playArea.width playArea.height con
                     in color Color.white area

renderBird : Bird -> Form
renderBird bird = let image = fittedImage 60 60 "assets/flappy.png"
                  in move (toFloat bird.x, toFloat bird.y) <| toForm image

renderObs : List Obstacle -> Form
renderObs obs = group <| List.map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = move (ob.x, ob.y) <| filled Color.blue <| rect ob.width (2 * ob.height)

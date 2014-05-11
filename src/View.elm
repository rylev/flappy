module View where

import Model (Obstacle, Game, Bird, playArea, Active, GameOver)

render : (Int, Int) -> Game -> Element
render winDim g = let renderScreen c = bg winDim <| renderPlayArea c
  in case g.state of
    Active -> renderScreen [renderBird g.bird, renderObs g.obstacles]
    GameOver -> renderScreen [toForm <| asText "Game Over!!"]

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

renderPlayArea : [Form] -> Element
renderPlayArea con = let area = collage playArea.width playArea.height con
                     in color white area

renderBird : Bird -> Form
renderBird bird = let image = fittedImage 60 60 "assets/flappy.png"
                  in move (toFloat bird.x, toFloat bird.y) <| toForm image

renderObs : [Obstacle] -> Form
renderObs obs = group <| map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = move (ob.x, ob.y) <| filled blue <| rect ob.width (2 * ob.height)

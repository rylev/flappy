module View where

import Model exposing (Obstacle, Game, Bird, playArea, GameState(Active,GameOver))
import Graphics.Collage as Collage exposing (Form)
import Graphics.Element as Element exposing (Element)
import Color exposing (green, white, blue)

type alias WindowDimensions = (Int, Int)

render : WindowDimensions -> Game -> Element
render winDim game = case game.state of
  Active -> renderScreen winDim [renderBird game.bird, renderObs game.obstacles]
  GameOver -> renderScreen winDim [Collage.toForm <| Element.show "Game Over!!"]

renderScreen : WindowDimensions -> List Form -> Element
renderScreen winDim contents = renderBackground winDim <| renderPlayArea contents

renderBackground : WindowDimensions -> Element -> Element
renderBackground (ww, wh) pa = Element.color green <| Element.container ww wh Element.middle pa

renderPlayArea : List Form -> Element
renderPlayArea con = let area = Collage.collage playArea.width playArea.height con
                     in Element.color white area

renderBird : Bird -> Form
renderBird bird = let image = Element.fittedImage 60 60 "assets/flappy.png"
                  in Collage.move (toFloat bird.x, toFloat bird.y) <| Collage.toForm image

renderObs : List Obstacle -> Form
renderObs obs = Collage.group <| List.map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = Collage.move (ob.x, ob.y) <| Collage.filled blue <| Collage.rect ob.width (2 * ob.height)

module View exposing (..)

import Model exposing (Obstacle, Game, Bird, playArea, GameState(Active,GameOver))
import Collage as Collage exposing (Form)
import Element as Element exposing (Element)
import Color exposing (green, white, blue)
import Text

type alias WindowDimensions = (Int, Int)

render : WindowDimensions -> Game -> Element
render winDim game = case game.state of
  Active -> renderActiveGame winDim game
  GameOver -> renderGameOver winDim game

renderActiveGame : WindowDimensions -> Game -> Element
renderActiveGame winDim game = let bird = renderBird game.bird
                                   obstables = renderObs game.obstacles
                                   points = renderPoints game.points
                               in renderScreen winDim [bird, obstables, points]

renderGameOver : WindowDimensions -> Game -> Element
renderGameOver winDim game = renderScreen winDim [gameOverText, renderFinalScore game.points]

gameOverText : Form
gameOverText = Text.fromString "Game Over!!"
  |> Text.height 40
  |> Collage.text

renderFinalScore : Int -> Form
renderFinalScore score = ("Final Score! " ++ toString score)
  |> Text.fromString
  |> Collage.text
  |> Collage.move (0, -100)

renderScreen : WindowDimensions -> List Form -> Element
renderScreen winDim contents = renderBackground winDim <| renderPlayArea contents

renderBackground : WindowDimensions -> Element -> Element
renderBackground (ww, wh) pa = Element.color blue <| Element.container ww wh Element.middle pa

renderPlayArea : List Form -> Element
renderPlayArea con = let area = Collage.collage playArea.width playArea.height con
                     in Element.color white area

renderBird : Bird -> Form
renderBird bird = let image = Element.fittedImage 60 60 "assets/flappy.png"
                  in Collage.move (toFloat bird.x, toFloat bird.y) <| Collage.toForm image

renderObs : List Obstacle -> Form
renderObs obs = Collage.group <| List.map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = Collage.move (ob.x, ob.y) <| Collage.filled green <| Collage.rect ob.width (2 * ob.height)

renderPoints : Int -> Form
renderPoints ps = Element.show ps
  |> Collage.toForm
  |> Collage.move (375.0, 200.0)

import Window
import Keyboard

main : Signal Element
main = lift render <| (foldp stepGame defaultGame input)

-- Input
data Event = Tick | Add Obstacle | UpArrow Bool

frameRate : Signal Time
frameRate = fps 30

input : Signal Event
input = merges [
          lift UpArrow keyboardInput,
          lift (\_ -> Tick) frameRate
        ]

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in lift isUp <| sampleOn frameRate Keyboard.arrows

-- Logic



stepGame : Event -> Game -> Game
stepGame e g = g

fly : Int -> Bird -> Bird
fly dir bird = let vy' = if dir > 0 then bird.vy + 2
                        else bird.vy - 2
                   y' = bird.y + vy'
               in {bird | y <- y', vy <- clamp -10 10 vy'}

-- Model
type Game = {}
type Bird = { x : Int, y : Int, vy : Float }
type Obstacle = { x : Int, y : Int, height : Int, width : Int }

defaultGame = {}
defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = 300, y = 0, height = 100, width = 40 }

-- View
render : Game -> Element
render g = bg (200, 200) <| playArea (renderBird defaultBird) (renderObs [defaultObstacle])

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

playArea : Form -> Form -> Element
playArea bird obs = color white <| collage 900 500 [bird, obs]

renderBird : Bird -> Form
renderBird bird = move (toFloat bird.x, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"

renderObs : [Obstacle] -> Form
renderObs _ = filled blue <| rect 10 10
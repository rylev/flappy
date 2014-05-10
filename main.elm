import Window
import Keyboard
import Random

main : Signal Element
main = lift render Window.dimensions ~ (foldp stepGame defaultGame input)

-- Input
data Event = Add Obstacle | Tick Bool

frameRate : Signal Time
frameRate = fps 30

obsInterval : Signal Time
obsInterval = every <| 2 * second

input : Signal Event
input = merges [
          lift Tick keyboardInput,
          lift Add createObstacle
        ]

createObstacle : Signal Obstacle
createObstacle = lift newObstacle <| Random.float obsInterval

keyboardInput : Signal Bool
keyboardInput = let isUp keys = keys.y == 1
                in lift isUp <| sampleOn frameRate Keyboard.arrows

-- Logic
stepGame : Event -> Game -> Game
stepGame e g = case e of
  Tick a -> let b = g.bird
                b' = if a then flyUp b else flyDown b
                obs' = filter visibleOb <| map shiftOb g.obstacles
               in { g | bird <- b', obstacles <- obs' }
  Add obs -> { g | obstacles <- obs :: g.obstacles }

visibleOb : Obstacle -> Bool
visibleOb ob = ob.x > -500

shiftOb : Obstacle -> Obstacle
shiftOb ob = { ob | x <- ob.x - 1 }

flyUp : Bird -> Bird
flyUp bird = let vy' = bird.vy + 2
             in fly vy' bird

flyDown : Bird -> Bird
flyDown bird = let vy' = bird.vy - 2
               in fly vy' bird

fly : Int -> Bird -> Bird
fly vy' bird = let y' = bird.y + vy'
               in {bird | y <- y', vy <- clamp -10 10 vy'}
-- Model
type Game = { bird : Bird, obstacles : [Obstacle] }
type Bird = { x : Int, y : Int, vy : Int }
type Obstacle = { x : Int, y : Int, height : Int, width : Int }

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [] }

defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = 300, y = 0, height = 100, width = 40 }

newObstacle : Float -> Obstacle
newObstacle f = { defaultObstacle | x <- clamp 0 500 (ceiling (f * (toFloat defaultObstacle.x))) }

-- View
render : (Int, Int) -> Game -> Element
render winDim g = bg winDim <| playArea (renderBird g.bird) (renderObs g.obstacles)

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

playArea : Form -> Form -> Element
playArea bird obs = color white <| collage 900 500 [bird, obs]

renderBird : Bird -> Form
renderBird bird = move (toFloat bird.x, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"

renderObs : [Obstacle] -> Form
renderObs obs = group <| map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = move (toFloat ob.x, toFloat ob.y) <| filled blue <| rect (toFloat ob.width) (toFloat ob.height)
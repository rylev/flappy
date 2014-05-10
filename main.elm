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
obsInterval = every <| 1 * second

input : Signal Event
input = merges [
          lift Tick keyboardInput,
          lift Add createObstacle
        ]

createObstacle : Signal Obstacle
createObstacle = newObstacle <~ Random.float obsInterval ~ topOrBottom

data Position = Top | Bottom | Skip

topOrBottom : Signal Position
topOrBottom = let position f = if | f < 0.4 -> Bottom
                                   | f >= 0.4 && f < 0.6 -> Skip
                                   | f >= 0.6 -> Top
              in lift position <| Random.float obsInterval

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
shiftOb ob = { ob | x <- ob.x - 10 }

flyUp : Bird -> Bird
flyUp bird = let vy' = bird.vy + 2
             in fly vy' bird

flyDown : Bird -> Bird
flyDown bird = let vy' = bird.vy - 2
               in fly vy' bird

fly : Int -> Bird -> Bird
fly vy bird = let y' = bird.y + vy
                  vy' = clamp -10 10 vy
               in {bird | y <- y', vy <- vy' }

-- Model
type Game = { bird : Bird, obstacles : [Obstacle] }
type Bird = { x : Int, y : Int, vy : Int }
type Obstacle = { x : Float, y : Float, height : Float, width : Float }
type PlayArea = { height : Int, width : Int }

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [] }

defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = 500, y = toFloat playAreaTop, height = 500, width = 60 }

playArea : PlayArea
playArea = { height = 500, width = 900 }

playAreaTop : Int
playAreaTop = div playArea.height 2

newObstacle : Float -> Position -> Obstacle
newObstacle f p = let o = defaultObstacle
                   in { o |
                     height <- clamp 0 500 (f * o.height),
                     y <- case p of
                            Bottom-> -o.y
                            Skip -> o.y + 10000000
                            Top -> o.y
                     }

-- View
render : (Int, Int) -> Game -> Element
render winDim g = bg winDim <| renderPlayArea (renderBird g.bird) (renderObs g.obstacles)

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

renderPlayArea : Form -> Form -> Element
renderPlayArea bird obs = color white <| collage playArea.width playArea.height [bird, obs]

renderBird : Bird -> Form
renderBird bird = move (toFloat bird.x, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"

renderObs : [Obstacle] -> Form
renderObs obs = group <| map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = move (ob.x, ob.y) <| filled blue <| rect ob.width ob.height
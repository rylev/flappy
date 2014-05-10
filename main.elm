import Window
import Keyboard
import Random
import Mouse

main : Signal Element
main = lift render Window.dimensions ~ (foldp stepGame defaultGame input)

-- Input
data Event = Add Obstacle | Tick Bool | Click

frameRate : Signal Time
frameRate = fps 30

obsInterval : Signal Time
obsInterval = every <| 1 * second

input : Signal Event
input = merges [
          lift Tick keyboardInput,
          lift Add createObstacle,
          lift (\_ -> Click) Mouse.clicks
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
stepGame e g = case g.state of
  Active -> stepPlay e g
  GameOver -> gameOver e g

gameOver : Event -> Game -> Game
gameOver e g = case e of
  Click -> defaultGame
  _ -> g

stepPlay : Event -> Game -> Game
stepPlay e g = case e of
  Tick a -> let b = g.bird
                b' = if a then flyUp b else flyDown b
                obs' = filter visibleOb <| map shiftOb g.obstacles
                state' = if anyCollision b' obs' then GameOver else Active
            in { g | bird <- b', obstacles <- obs', state <- state' }
  Add obs -> { g | obstacles <- obs :: g.obstacles }
  Click -> g

anyCollision : Bird -> [Obstacle] -> Bool
anyCollision b obs = let bx = toFloat b.x
                         by = toFloat b.y
                         outOfBounds b = b.y > playAreaTop || b.y < playAreaBottom
                         sameX b o = bx == o.x
                         between x l u = x >= l && x <= u
                         sameY b o = between by (o.y - o.height) (o.y + o.height)
                         collision o = b `sameX` o && (outOfBounds b || b `sameY` o)
                     in any collision obs

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
type Game = { bird : Bird, obstacles : [Obstacle], state : State }
type Bird = { x : Int, y : Int, vy : Int }
type Obstacle = { x : Float, y : Float, height : Float, width : Float }
type PlayArea = { height : Int, width : Int }

data State = Active | GameOver

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [], state = Active }

defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = 500, y = toFloat playAreaTop, height = 300, width = 60 }

playArea : PlayArea
playArea = { height = 500, width = 900 }

playAreaTop : Int
playAreaTop = div playArea.height 2

playAreaBottom : Int
playAreaBottom = 0 - div playArea.height 2

newObstacle : Float -> Position -> Obstacle
newObstacle f p = let o = defaultObstacle
                   in { o |
                     height <- clamp 0 300 (f * o.height),
                     y <- case p of
                            Bottom-> -o.y
                            Skip -> o.y + 10000000
                            Top -> o.y
                     }

-- View
render : (Int, Int) -> Game -> Element
render winDim g = let renderContent content = bg winDim <| renderPlayArea content
  in case g.state of
    Active -> renderContent [renderBird g.bird, renderObs g.obstacles]
    GameOver -> renderContent [toForm <| asText "Game Over!!"]

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

renderPlayArea : [Form] -> Element
renderPlayArea content = color white <| collage playArea.width playArea.height content

renderBird : Bird -> Form
renderBird bird = move (toFloat bird.x, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"

renderObs : [Obstacle] -> Form
renderObs obs = group <| map renderOb obs

renderOb : Obstacle -> Form
renderOb ob = move (ob.x, ob.y) <| filled blue <| rect ob.width (2 * ob.height)
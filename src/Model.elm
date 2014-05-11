module Model where

type Game = { bird : Bird, obstacles : [Obstacle], state : GameState }
type Bird = { x : Int, y : Int, vy : Int }
type Obstacle = { x : Float, y : Float, height : Float, width : Float }
type PlayArea = { height : Int, width : Int }

data GameState = Active | GameOver
data Position = Top | Bottom | Skip

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [], state = Active }

defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = toFloat playAreaRight,
                    y = toFloat playAreaTop,
                    height = 300,
                    width = 60 }

playArea : PlayArea
playArea = { height = 500, width = 900 }

playAreaTop : Int
playAreaTop = div playArea.height 2

playAreaRight : Int
playAreaRight = div playArea.width 2

playAreaLeft : Int
playAreaLeft = 0 - playAreaRight

playAreaBottom : Int
playAreaBottom = 0 - playAreaTop

newObstacle : Float -> Position -> Obstacle
newObstacle f p = let o = defaultObstacle
                   in { o |
                     height <- clamp 0 300 (f * o.height),
                     y <- case p of
                            Bottom-> -o.y
                            Skip -> o.y + 10000000
                            Top -> o.y
                     }

module Model where

import Random

type alias Game = { bird : Bird, obstacles : List Obstacle, state : GameState }
type alias Bird = { x : Int, y : Int, vy : Int }
type alias Obstacle = { x : Float, y : Float, height : Float, width : Float, seed: Random.Seed }
type alias PlayArea = { height : Int, width : Int }

type GameState = Active | GameOver
type Position = Top | Bottom | Skip

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [], state = Active }

defaultBird : Bird
defaultBird = {x = -300, y = 0, vy = 0}

defaultObstacle : Obstacle
defaultObstacle = { x = toFloat playAreaRight,
                    y = toFloat playAreaTop,
                    height = 300,
                    width = 60,
                    seed  = Random.initialSeed 10}

playArea : PlayArea
playArea = { height = 500, width = 900 }

playAreaTop : Int
playAreaTop = playArea.height // 2

playAreaRight : Int
playAreaRight = playArea.width // 2

playAreaLeft : Int
playAreaLeft = 0 - playAreaRight

playAreaBottom : Int
playAreaBottom = 0 - playAreaTop

newObstacle : Float -> Position -> Random.Seed -> Obstacle
newObstacle f p s = let o = defaultObstacle
                    in { o | height <- clamp 0 300 (f * o.height),
                             y      <- newPosition p o,
                             seed   <- s}

newPosition : Position -> Obstacle -> Float
newPosition p o = case p of
                    Bottom-> -o.y
                    Skip -> o.y + 10000000
                    Top -> o.y


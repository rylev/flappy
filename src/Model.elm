module Model where

type alias Game = { bird : Bird, obstacles : List Obstacle, state : GameState, points: Int }
type alias Bird = { x : Int, y : Int, vy : Int }
type alias Obstacle = { x : Float, y : Float, height : Float, width : Float }
type alias PlayArea = { height : Int, width : Int }

type GameState = Active | GameOver
type Position = Top | Bottom | Skip

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [], state = Active, points = 0 }

defaultBird : Bird
defaultBird = { x = -300, y = 0, vy = 0 }

defaultObstacle : Obstacle
defaultObstacle = { x = toFloat playAreaRight,
                    y = toFloat playAreaTop,
                    height = 300,
                    width = 60 }

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

newObstacle : Float -> Position -> Obstacle
newObstacle f p = let o = defaultObstacle
                  in { o | height <- clamp 0 300 (f * o.height),
                           y      <- newPosition p o }

newPosition : Position -> Obstacle -> Float
newPosition p o = case p of
                    Bottom-> -o.y
                    Skip -> o.y + 10000000
                    Top -> o.y

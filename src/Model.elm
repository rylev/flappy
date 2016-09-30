module Model exposing (..)

type alias Game = { bird : Bird, obstacles : List Obstacle, state : GameState, points: Int, arrowPressed: Bool }
type alias Bird = { x : Int, y : Int, vy : Int }
type alias Obstacle = { x : Float, y : Float, height : Float, width : Float , position: Position }
type alias PlayArea = { height : Int, width : Int }

type GameState = Active | GameOver
type Position = Top | Bottom

defaultGame : Game
defaultGame = { bird = defaultBird, obstacles = [], state = Active, points = 0, arrowPressed = False }

defaultBird : Bird
defaultBird = { x = -300, y = 0, vy = 0 }

defaultObstacle : Obstacle
defaultObstacle = { x = toFloat playAreaRight,
                    y = toFloat playAreaTop,
                    height = 300,
                    width = 60,
                    position = Top }

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
newObstacle scaleFactor position =
  { defaultObstacle | height   = clamp 100 270 (scaleFactor * defaultObstacle.height),
                      position = position,
                      y        = positionAsFloat position }

positionAsFloat : Position -> Float
positionAsFloat position = case position of
  Bottom-> -defaultObstacle.y
  Top -> defaultObstacle.y

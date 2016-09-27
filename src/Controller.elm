module Controller exposing (..)

import Model exposing (Obstacle, GameState(Active,GameOver), Game, Bird)
import Random exposing (Seed)
import Maybe exposing (Maybe(Just,Nothing))

type Event = Add Seed | Tick | Click | UpArrowPressed | UpArrowReleased

stepGame : Event -> Game -> Game
stepGame e g = case g.state of
  Active -> stepPlay e g
  GameOver -> stepGameOver e g

stepGameOver : Event -> Game -> Game
stepGameOver e g = case e of
  Click -> Model.defaultGame
  _ -> g

stepPlay : Event -> Game -> Game
stepPlay event game = case event of
  UpArrowPressed -> pressArrow game
  UpArrowReleased -> releaseArrow game
  Tick -> tickGame game
  Add seed -> updateObstacles game seed
  Click -> game

pressArrow : Game -> Game
pressArrow game = { game | arrowPressed = True }

releaseArrow : Game -> Game
releaseArrow game = { game | arrowPressed = False }

updateObstacles : Game -> Seed -> Game
updateObstacles game seed = case newObstacle seed of
  Just o -> { game | obstacles = o :: game.obstacles }
  Nothing -> game

newObstacle : Seed -> Maybe Obstacle
newObstacle seed = let generator = Random.float 0 1
                       (f1, seed') = Random.step generator seed
                       (f2,_) = Random.step generator seed'
                       position = toPosition f2
                    in Maybe.map (Model.newObstacle f1) position

toPosition : Float -> Maybe Model.Position
toPosition f = if      f < 0.40              then Just Model.Bottom
               else if f >= 0.40 && f < 0.6  then Nothing
               else                               Just Model.Top

tickGame : Game -> Game
tickGame game =
  let b = game.bird
      b' = if game.arrowPressed then flyUp b else flyDown b
      shiftOb ob = { ob | x = ob.x - 10.0 }
      obIsVisible ob = ob.x > (toFloat Model.playAreaLeft) - ob.width
      obs' = List.filter obIsVisible <| List.map shiftOb game.obstacles
      state' = if anyCollision b' obs' then GameOver else Active
      points' = game.points + 1
  in { game | bird = b', obstacles = obs', state = state', points = points' }

anyCollision : Bird -> List Obstacle -> Bool
anyCollision bird obs = List.any (collision bird) obs

collision : Bird -> Obstacle -> Bool
collision bird obstacle = bird `hasSameXPosition` obstacle && bird `hasSameYPosition` obstacle

hasSameYPosition : Bird -> Obstacle -> Bool
hasSameYPosition bird obstacle =
  let yPosition = toFloat bird.y
      obstacleBottom = obstacle.y - obstacle.height
      obstacleTop = obstacle.y + obstacle.height
  in case obstacle.position of
    Model.Top -> yPosition > obstacleBottom
    Model.Bottom -> yPosition < obstacleTop

hasSameXPosition : Bird -> Obstacle -> Bool
hasSameXPosition bird obstacle =
  let xPosition = toFloat bird.x
      obstacleFront = (obstacle.x - (obstacle.width / 2))
      obstacleBack = (obstacle.x + (obstacle.width / 2))
  in between xPosition obstacleFront obstacleBack

isOutOfBounds : Bird -> Bool
isOutOfBounds bird = bird.y > Model.playAreaTop || bird.y < Model.playAreaBottom

between : Float -> Float -> Float -> Bool
between a b c = a >= b && a <= c

flyUp : Bird -> Bird
flyUp bird = let vy' = bird.vy + 2
             in fly vy' bird

flyDown : Bird -> Bird
flyDown bird = let vy' = bird.vy - 2
               in fly vy' bird

fly : Int -> Bird -> Bird
fly vy bird = let y' = bird.y + vy
                  vy' = clamp -10 10 vy
              in { bird | y = y', vy = vy' }

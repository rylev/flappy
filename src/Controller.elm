module Controller where

import Model exposing (Obstacle, GameState(Active,GameOver), Game, Bird)
import Random exposing (Seed)

type Event = Add Seed | Tick Bool | Click

stepGame : Event -> Game -> Game
stepGame e g = case g.state of
  Active -> stepPlay e g
  GameOver -> gameOver e g

gameOver : Event -> Game -> Game
gameOver e g = case e of
  Click -> Model.defaultGame
  _ -> g

stepPlay : Event -> Game -> Game
stepPlay e g = case e of
  Tick keyIsPressed -> tickGame keyIsPressed g
  Add seed -> { g | obstacles <- (newObstacle seed) :: g.obstacles }
  Click -> g

newObstacle : Seed -> Obstacle
newObstacle seed = let generator = Random.float 0 1
                       (f1, seed') = Random.generate generator seed
                       (f2,_) = Random.generate generator seed'
                    in Model.newObstacle f1 (toPosition f2)

toPosition : Float -> Model.Position
toPosition f = if | f < 0.40              -> Model.Bottom
                  | f >= 0.40 && f < 0.6  -> Model.Skip
                  | f >= 0.60             -> Model.Top

tickGame : Bool -> Game -> Game
tickGame keyIsPressed game =
  let b = game.bird
      b' = if keyIsPressed then flyUp b else flyDown b
      shiftOb ob = { ob | x <- ob.x - 10.0 }
      obIsVisible ob = ob.x > (toFloat Model.playAreaLeft) - ob.width
      obs' = List.filter obIsVisible <| List.map shiftOb game.obstacles
      state' = if anyCollision b' obs' then GameOver else Active
      points' = game.points + 1
  in { game | bird <- b', obstacles <- obs', state <- state', points <- points' }

anyCollision : Bird -> List Obstacle -> Bool
anyCollision bird obs = List.any (\ob -> collision bird ob) obs

collision : Bird -> Obstacle -> Bool
collision bird obstacle = bird `hasSameXPosition` obstacle && (isOutOfBounds bird || bird `hasSameYPosition` obstacle)

hasSameYPosition : Bird -> Obstacle -> Bool
hasSameYPosition bird obstacle = let yPosition = toFloat bird.y
                                     obstacleBottom = (obstacle.y - obstacle.height)
                                     obstacleTop = (obstacle.y + obstacle.height)
                                 in between yPosition obstacleBottom obstacleTop

hasSameXPosition : Bird -> Obstacle -> Bool
hasSameXPosition bird obstacle = let xPosition = toFloat bird.x
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
              in { bird | y <- y', vy <- vy' }

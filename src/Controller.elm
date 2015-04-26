module Controller where

import Model exposing (Obstacle, GameState(Active,GameOver), Game, Bird)

type Event = Add Obstacle | Tick Bool | Click

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
  Tick keyIsPressed -> let b = g.bird
                           b' = if keyIsPressed then flyUp b else flyDown b
                           shiftOb ob = { ob | x <- ob.x - 10.0 }
                           obIsVisible ob = ob.x > (toFloat Model.playAreaLeft) - ob.width
                           obs' = List.filter obIsVisible <| List.map shiftOb g.obstacles
                           state' = if anyCollision b' obs' then GameOver else Active
            in { g | bird <- b', obstacles <- obs', state <- state' }
  Add obs -> { g | obstacles <- obs :: g.obstacles }
  Click -> g

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

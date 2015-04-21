module Controller where

import Model exposing (..)

type Event = Add Obstacle | Tick Bool | Click

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
                shiftOb ob = { ob | x <- ob.x - 10.0 }
                visibleOb ob = ob.x > (toFloat playAreaLeft) - ob.width
                obs' = List.filter visibleOb <| List.map shiftOb g.obstacles
                state' = if anyCollision b' obs' then GameOver else Active
            in { g | bird <- b', obstacles <- obs', state <- state' }
  Add obs -> { g | obstacles <- obs :: g.obstacles }
  Click -> g

anyCollision : Bird -> List Obstacle -> Bool
anyCollision b obs = let bx = toFloat b.x
                         by = toFloat b.y
                         outOfBounds b = b.y > playAreaTop || b.y < playAreaBottom
                         sameX b o = bx == (o.x - (o.width / 2))
                         between x l u = x >= l && x <= u
                         sameY b o = between by (o.y - o.height) (o.y + o.height)
                         collision o = b `sameX` o && (outOfBounds b || b `sameY` o)
                     in List.any collision obs

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

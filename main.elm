import Window
import Keyboard

main : Signal Element
main = render <~ Window.dimensions ~ bird

-- Input
input : Signal Int
input = let getY keys = keys.y
        in lift getY <| sampleOn (fps 30) Keyboard.arrows

-- Logic
bird : Signal Bird
bird = foldp fly defaultBird input

fly : Int -> Bird -> Bird
fly dir bird = let vy' = if dir > 0 then bird.vy + 2
                        else bird.vy - 2
                   y' = bird.y + vy'
               in {bird | y <- y', vy <- clamp -10 10 vy'}

-- Model
type Bird = { y : Int, vy : Float }

defaultBird : Bird
defaultBird = {y = 0, vy = 0}

-- View
render : (Int, Int) -> Bird -> Element
render wds bird = bg wds <| playArea <| renderBird bird

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

playArea : Form -> Element
playArea bird = color white <| collage 900 500 [bird]

renderBird : Bird -> Form
renderBird bird = move (0, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"
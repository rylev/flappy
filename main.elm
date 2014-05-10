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
fly dir bird = if dir == 1 then {bird | y <- bird.y + 20} else { bird | y <- bird.y - 20 }

-- Model
type Bird = { x : Int, y : Int }

defaultBird : Bird
defaultBird = {x = 0, y = 0}

-- View

render : (Int, Int) -> Bird -> Element
render wds bird = bg wds <| playArea <| renderBird bird

bg : (Int, Int) -> Element -> Element
bg (ww, wh) pa = color green <| container ww wh middle pa

playArea : Form -> Element
playArea bird = color white <| collage 900 500 [bird]


renderBird : Bird -> Form
renderBird bird = move (toFloat bird.x, toFloat bird.y) <| toForm <| fittedImage 60 60 "flappy.png"
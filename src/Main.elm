module Main exposing (main)

import Html.App as App
import Element

import View
import Model
import Controller
import Input

main = App.program
  { init = (Model.defaultGame, Cmd.none)
  , view = Element.toHtml << (View.render (1270, 600))
  , update = \event game -> (Controller.stepGame event game, Cmd.none)
  , subscriptions = \_ -> Input.input
  }
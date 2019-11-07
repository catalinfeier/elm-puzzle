module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = {
  integer: Int,
  content: String,
  tiles: Array.Array(Int)
  }

init : Model
init = {
  integer = 0,
  content = "",
  tiles = Array.fromList [2, 3, 1, 4, 5, 6, 7, 8, 0]
  }

type Msg = Increment | Decrement | Reset | Change String

update msg model =
  case msg of
    Increment ->
      { model | integer = model.integer + 1 }

    Decrement ->
      { model | integer = model.integer + 1 }

    Reset ->
      init

    Change newContent ->
      {model | content = newContent}

tileClass index value =
  if (index + 1) == value then 
     "tile correct"
  else if value == 0 then
    "tile"
  else 
    "tile incorrect"

displayTile index value = 
    div [class  (tileClass index value)] [text (String.fromInt value)]

view : Model -> Html Msg
view model =
  div []
    [ 
    div [class "puzzleContainer"] (Array.toList (Array.indexedMap displayTile model.tiles))
    ]
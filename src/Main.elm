module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = {
  integer: Int,
  tiles: (Array.Array(Int), Int)
  }

init : Model
init = {
  integer = 0,
  tiles = shuffle
  }

valid : Array.Array(Int)
valid = Array.fromList [5, 2, 8, 4, 1, 7, 0, 3, 6]
type Msg = ClickTile Int Int| Reset

update msg model =
  let tiles = Tuple.first model.tiles
      zeroPos = Tuple.second model.tiles
  in
  case msg of
    ClickTile index value->
      if canMove index value zeroPos then 
        { model | integer = model.integer + 1, tiles = (Array.set zeroPos value (Array.set index 0 tiles), index)}
      else model

    Reset ->
      init

shuffle =
  (valid, 6)

canMove index value zeroPos =
  if value == 0 then False
  else if zeroPos == 8 && (index == 5 || index == 7)  then True
  else if zeroPos == 7 && (index == 4 || index == 6 || index == 8) then True
  else if zeroPos == 6 && (index == 3 || index == 7) then True
  else if zeroPos == 5 && (index == 4 || index == 8 || index == 2) then True
  else if zeroPos == 4 && (index == 3 || index == 1 || index == 5 || index == 7) then True
  else if zeroPos == 3 && (index == 0 || index == 4 || index == 6) then True
  else if zeroPos == 2 && (index == 1 || index == 5) then True
  else if zeroPos == 1 && (index == 0 || index == 2 || index == 4) then True
  else if zeroPos == 0 && (index == 1 || index == 3) then True
  else False

tileClass index value =
  if (index + 1) == value then 
     "tile correct"
  else if value == 0 then
    "tile zero"
  else 
    "tile incorrect"

displayTile index value = 
    button [class (tileClass index value), onClick (ClickTile index value)] [text (String.fromInt value)]

view : Model -> Html Msg
view model =
  div []
    [ 
    div [class "puzzleContainer"] 
          (Array.toList (Array.indexedMap displayTile (Tuple.first model.tiles)))
    , div [] [ text ("Counter:" ++  String.fromInt model.integer) ]
    , button [onClick Reset] [ text "Reset" ]
    ]
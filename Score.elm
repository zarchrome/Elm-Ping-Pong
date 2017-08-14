import Html exposing (text)
import Tuple exposing (..)

type Player
  = Player1
  | Player2

type alias Score =
  { game : ( Int, Int )
  , match: ( Int, Int )
  }

wonGame : Score -> ((Int, Int) -> Int) -> ((Int, Int) -> Int) -> Bool
wonGame score which other =
  (which score.game) >= 21 && (which score.game) > ((other score.game) + 1)

rebalance : Score -> Score
rebalance score =
  if wonGame score first second then
    { score |
        game = (0,0),
        match = (,) ((first score.match) + 1) (second score.match)
    }
  else if wonGame score second first then
    { score |
        game = (0,0),
        match = (,) (first score.match) ((second score.match) + 1)
    }
  else
    score

add1 : Score -> Player -> Score
add1 score player =
  case player of
    Player1 ->
      { score | game = (,) (first score.game + 1) (second score.game) }
    Player2 ->
      { score | game = (,) (first score.game) (second score.game + 1) }

addPoint : Score -> Player -> Score
addPoint score player =
  let
    newScore = rebalance (add1 score player)
  in
  if first newScore.match >= 2 then
    { score |
      game = (,) 0 0,
      match = (2, second newScore.match)
    }
  else if second newScore.match >= 2 then
    { score |
      game = (,) 0 0,
      match = (first newScore.match, 2)
    }
  else
    newScore

printScore : Score -> String
printScore score =
  "Yay " ++
  toString (first score.game) ++
  ", " ++
  toString (second score.game) ++
  ", " ++
  toString (first score.match) ++
  ", " ++
  toString (second score.match)

s = Score (0,0) (0,0)

main =
  let
    score = Score (20,20) (1,0)
    nScore = addPoint score Player1
    bScore = rebalance nScore
  in
    text (printScore bScore)

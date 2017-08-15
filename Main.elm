import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tuple exposing (..)

-- API for coin flip to pick starting serve
-- https://www.random.org/integers/?num=1&min=0&max=1&col=3&base=10&format=plain&rnd=new

main =
  Html.beginnerProgram
  { model = initialModel
  , view = view
  , update = update
  }

-- MODEL

type Player
  = Player1
  | Player2

type alias Score =
  { game : ( Int, Int )
  , match: ( Int, Int )
  }

type alias Model =
  { score: Score
  , matchWinner : Maybe Player
  }

newScore = Score (0,0) (0,0)

initialModel : Model
initialModel =
  Model newScore Nothing

-- UPDATE

type Msg
  = NewPoint Player
  | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewPoint player ->
      { model |
          score = addPoint model.score player
      }
    Reset ->
      initialModel

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

-- VIEW

view : Model -> Html Msg
view model = 
  Html.div []
  [
    Html.node "link"[ href "style.css", rel "stylesheet" ] [],
    Html.node "header" []
    [
      Html.h1 [] [ Html.text "Ping Pong Score" ] --,
      -- Html.h3 [] [ Html.text (winnerText model) ]
    ],
    Html.div [ id "players" ]
    [
      Html.div [ id "player1" ]
      [
        Html.p [] [ Html.text "Player 1" ],
        Html.p [] [ Html.text (printScore model.score) ],
        -- Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player1 ) )],
        -- Html.p [] [ Html.text ( toString model.matchScore.player1 ) ],
        Html.button [ onClick (NewPoint Player1) ] [ Html.text "+" ]
      ],
      Html.div [ id "player2" ]
      [
        Html.p [] [ Html.text "Player 2" ],
        Html.p [] [ Html.text (printScore model.score) ],
        -- Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player2 ) )],
        -- Html.p [] [ Html.text ( toString model.matchScore.player2 ) ],
        Html.button [ onClick (NewPoint Player2) ] [ Html.text "+" ]
      ]
    ],
    Html.footer []
    [
      Html.button [ onClick Reset ] [ Html.text "Reset" ]
    ]
  ]

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


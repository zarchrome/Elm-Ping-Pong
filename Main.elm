import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

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

type alias Score = Int

type alias GameScore =
  { player1 : Score
  , player2 : Score
  }

type alias MatchScore =
  { player1 : Score
  , player2 : Score
  }

type alias Model =
  { gameScore : GameScore
  , matchScore : MatchScore
  , matchWinner : Maybe Player
  }

newGameScore = GameScore 0 0

newMatchScore = MatchScore 0 0

initialModel : Model
initialModel =
  Model newGameScore newMatchScore Nothing


-- UPDATE

type Msg
  = Player1Point
  | Player2Point
  | Reset

matchWinner : Model -> Maybe Player
matchWinner model =
  if model.matchScore.player1 == 2 || model.matchScore.player1 == 1 && (gameWinner model) == Just Player1 then
    Just Player1
  else if model.matchScore.player2 == 2 || model.matchScore.player2 == 1 && (gameWinner model) == Just Player1 then
    Just Player2
  else
    Nothing

wonGame: Score -> Score -> Bool
wonGame score1 score2 =
  if score1 > 20 && score1 > score2 + 1 then
    True
  else
    False

gameWinner : Model -> Maybe Player
gameWinner model =
  if wonGame model.gameScore.player1 model.gameScore.player2 then
    Just Player1
  else if wonGame model.gameScore.player2 model.gameScore.player1 then
    Just Player2
  else
    Nothing

winnerText : Model -> String
winnerText model =
  case model.matchWinner of
    Just Player1 -> "Congrats Player 1"
    Just Player2 -> "Good Job Player 2"
    Nothing -> ""

addItUp intermediateModel =
  case matchWinner intermediateModel of
    Just Player1 ->
      { intermediateModel | matchWinner = Just Player1 }
    Just Player2 ->
      { intermediateModel | matchWinner = Just Player2 }
    Nothing ->
      case gameWinner intermediateModel of
        Just Player1 ->
          let
            yayMatchScore = MatchScore ( intermediateModel.matchScore.player1 + 1 ) intermediateModel.matchScore.player2
          in
          Model newGameScore yayMatchScore Nothing
        Just Player2 ->
          let
            yayMatchScore = MatchScore intermediateModel.matchScore.player1 ( intermediateModel.matchScore.player2 + 1 )
          in
          Model newGameScore yayMatchScore Nothing
        Nothing ->
          intermediateModel

update : Msg -> Model -> Model
update msg model =
  case model.matchWinner of
    Just Player1 ->
      model
    Just Player2 ->
      model
    Nothing ->
      case msg of
        Player1Point ->
          let
            gS = model.gameScore
            newScore = { gS | player1 = gS.player1 + 1 }
            intermediateModel = { model | gameScore = newScore }
          in
          addItUp intermediateModel
        Player2Point ->
          let
            gS = model.gameScore
            newScore = { gS | player2 = gS.player2 + 1 }
            intermediateModel = { model | gameScore = newScore }
          in
          addItUp intermediateModel
        Reset ->
          initialModel

-- VIEW

view : Model -> Html Msg
view model = 
  Html.div []
  [
    Html.div [ id "winner" ]
    [
      Html.h3 [] [ Html.text (winnerText model) ]
    ],
    Html.div [ id "player1" ]
    [
      Html.h1 [] [ Html.text "p1" ],
      Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player1 ) )],
      Html.p [] [ Html.text ( "match: " ++ ( toString model.matchScore.player1 ) )],
      Html.button [ onClick Player1Point ] [ Html.text "+" ]
    ],
    Html.div [ id "player2" ]
    [
      Html.h1 [] [ Html.text "p2" ],
      Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player2 ) )],
      Html.p [] [ Html.text ( "match: " ++ ( toString model.matchScore.player2 ) )],
      Html.button [ onClick Player2Point ] [ Html.text "+" ]
      
    ]
  ]

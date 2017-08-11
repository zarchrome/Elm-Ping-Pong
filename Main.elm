import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

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

type alias Model =
  { player1GameScore : Int
  , player2GameScore : Int
  , player1MatchScore : Int
  , player2MatchScore : Int
  , matchWinner : Maybe Player
  }

initialModel : Model
initialModel =
  Model 0 0 0 0 Nothing


-- UPDATE

type Msg
  = Player1Point
  | Player2Point
  | Reset

matchWinner : Model -> Maybe Player
matchWinner model =
  if model.player1MatchScore == 2 || model.player1MatchScore == 1 && (gameWinner model) == Just Player1 then
    Just Player1
  else if model.player2MatchScore == 2 || model.player2MatchScore == 1 && (gameWinner model) == Just Player2 then
    Just Player2
  else
    Nothing

gameWinner : Model -> Maybe Player
gameWinner model =
  if model.player1GameScore > 20 && model.player1GameScore > model.player2GameScore + 1 then
    Just Player1
  else if model.player2GameScore > 20 && model.player2GameScore > model.player1GameScore + 1 then
    Just Player2
  else
    Nothing

winnerText : Model -> String
winnerText model =
  case model.matchWinner of
    Just Player1 -> "Congrats Player 1"
    Just Player2 -> "Good Job Player 2"
    Nothing -> ""

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
            intermediateModel = { model | player1GameScore = model.player1GameScore + 1 }
          in
          case matchWinner intermediateModel of
            Just Player1 ->
              { intermediateModel | matchWinner = Just Player1 }
            Just Player2 ->
              { intermediateModel | matchWinner = Just Player2 }
            Nothing ->
              case gameWinner intermediateModel of
                Just Player1 ->
                  Model 0 0 ( model.player1MatchScore + 1) model.player2MatchScore Nothing
                Just Player2 ->
                  Model 0 0 model.player1MatchScore ( model.player2MatchScore + 1) Nothing
                Nothing ->
                  intermediateModel
        Player2Point ->
          let
            intermediateModel = { model | player2GameScore = model.player2GameScore + 1 }
          in
          case matchWinner intermediateModel of
            Just Player1 ->
              { intermediateModel | matchWinner = Just Player1 }
            Just Player2 ->
              { intermediateModel | matchWinner = Just Player2 }
            Nothing ->
              case gameWinner intermediateModel of
                Just Player1 ->
                  Model 0 0 ( model.player1MatchScore + 1) model.player2MatchScore Nothing
                Just Player2 ->
                  Model 0 0 model.player1MatchScore ( model.player2MatchScore + 1) Nothing
                Nothing ->
                  intermediateModel
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
      Html.p [] [ Html.text ( "game: " ++ ( toString model.player1GameScore ) )],
      Html.p [] [ Html.text ( "match: " ++ ( toString model.player1MatchScore ) )],
      Html.button [ onClick Player1Point ] [ Html.text "+" ]
    ],
    Html.div [ id "player2" ]
    [
      Html.h1 [] [ Html.text "p2" ],
      Html.p [] [ Html.text ( "game: " ++ ( toString model.player2GameScore ) )],
      Html.p [] [ Html.text ( "match: " ++ ( toString model.player2MatchScore ) )],
      Html.button [ onClick Player2Point ] [ Html.text "+" ]
      
    ]
  ]

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

type alias Model =
  { player1GameScore : Int
  , player2GameScore : Int
  , player1MatchScore : Int
  , player2MatchScore : Int
  }

initialModel : Model
initialModel =
  Model 0 0 0 0


-- UPDATE

type Msg
  = Player1Point
  | Player2Point
  | Reset

type Player
  = Player1
  | Player2

gameWinner : Model -> Maybe Player
gameWinner model =
  if model.player1GameScore > 20 && model.player1GameScore > model.player2GameScore + 1 then
    Just Player1
  else if model.player2GameScore > 20 && model.player2GameScore > model.player1GameScore + 1 then
    Just Player2
  else
    Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    Player1Point ->
      let
        intermediateModel = { model | player1GameScore = model.player1GameScore + 1 }
      in
      case gameWinner intermediateModel of
        Just Player1 ->
          Model 0 0 ( model.player1MatchScore + 1) model.player2MatchScore
        Just Player2 ->
          Model 0 0 model.player1MatchScore ( model.player2MatchScore + 1)
        Nothing ->
          intermediateModel
    Player2Point ->
      let
        intermediateModel = { model | player2GameScore = model.player2GameScore + 1 }
      in
      case gameWinner intermediateModel of
        Just Player1 ->
          Model 0 0 ( model.player1MatchScore + 1) model.player2MatchScore
        Just Player2 ->
          Model 0 0 model.player1MatchScore ( model.player2MatchScore + 1)
        Nothing ->
          intermediateModel
    Reset ->
      initialModel

-- VIEW

view : Model -> Html Msg
view model = 
  Html.div []
  [
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

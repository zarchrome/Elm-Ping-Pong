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

type alias GameScore =
  { player1 : Int
  , player2 : Int
  }

type alias MatchScore =
  { player1 : Int
  , player2 : Int
  }

type alias CompositeScore =
  {
    game : GameScore
  , match: MatchScore
  }

type alias Model =
  { gameScore : GameScore
  , matchScore : MatchScore
  , compositeScore : CompositeScore
  , matchWinner : Maybe Player
  }

newGameScore = GameScore 0 0
newMatchScore = MatchScore 0 0
newCompositeScore = CompositeScore newGameScore newMatchScore

initialModel : Model
initialModel =
  Model newGameScore newMatchScore newCompositeScore Nothing

isJust : Maybe a -> Bool
isJust m = 
  case m of
    Just _ ->
      True
    Nothing ->
      False

-- UPDATE

type Msg
  = NewPoint Player
  | Reset

wonGame: Int -> Int -> Bool
wonGame score1 score2 =
  if score1 > 20 && score1 > score2 + 1 then
    True
  else
    False

matchWinner : Model -> Maybe Player
matchWinner model =
  if model.matchScore.player1 == 2 || model.matchScore.player1 == 1 && (gameWinner model) == Just Player1 then
    Just Player1
  else if model.matchScore.player2 == 2 || model.matchScore.player2 == 1 && (gameWinner model) == Just Player1 then
    Just Player2
  else
    Nothing

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

addGamePoint : GameScore -> Player -> GameScore
addGamePoint gameScore player =
  case player of
    Player1 -> { gameScore | player1 = gameScore.player1 + 1 }
    Player2 -> { gameScore | player2 = gameScore.player2 + 1 }

addMatchPoint : MatchScore -> Player -> MatchScore
addMatchPoint matchScore player =
  case player of
    Player1 -> { matchScore | player1 = matchScore.player1 + 1 }
    Player2 -> { matchScore | player2 = matchScore.player2 + 1 }

addItUp intermediateModel =
  let
    gWinner = gameWinner intermediateModel
    mWinner = matchWinner intermediateModel
  in
  case mWinner of
    Just player ->
      { intermediateModel | matchWinner = Just player }
    Nothing ->
      case gWinner of
        Just player ->
          let
            yayMatchScore = addMatchPoint intermediateModel.matchScore player
          in
          Model newGameScore yayMatchScore newCompositeScore Nothing
        Nothing ->
          intermediateModel

update : Msg -> Model -> Model
update msg model =
  if isJust model.matchWinner then model
  else case msg of
      NewPoint player ->
        let
          newScore = addGamePoint model.gameScore player
          intermediateModel = { model | gameScore = newScore }
        in
        addItUp intermediateModel
      Reset ->
        initialModel

n_hasGameWinner : CompositeScore -> Bool
n_hasGameWinner compositeScore =
  let
    p1 = compositeScore.game.player1
    p2 = compositeScore.game.player1
  in
    if p1 > 20 && p1 > p2 + 1 then
      True
    else if p2 > 20 && p2 > p1 + 1 then
      True
    else
      False

addToCompositeScore : CompositeScore -> Player -> CompositeScore
addToCompositeScore s p =
  if n_hasGameWinner s then
    s -- really convert to match
    
  else
    case p of
      Player1 -> 
        let
          game = s.game
          ngs = { game | player1 = game.player1 + 1 }
        in
          { s | game = ngs }
      Player2 -> 
        let
          game = s.game
          ngs = { game | player1 = game.player2 + 1 }
        in
          { s | game = ngs }

-- VIEW

view : Model -> Html Msg
view model = 
  Html.div []
  [
    Html.node "link"[ href "style.css", rel "stylesheet" ] [],
    Html.node "header" []
    [
      Html.h1 [] [ Html.text "Ping Pong Score" ],
      Html.h3 [] [ Html.text (winnerText model) ]
    ],
    Html.div [ id "players" ]
    [
      Html.div [ id "player1" ]
      [
        Html.p [] [ Html.text "Player 1" ],
        Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player1 ) )],
        Html.p [] [ Html.text ( toString model.matchScore.player1 ) ],
        Html.button [ onClick (NewPoint Player1) ] [ Html.text "+" ]
      ],
      Html.div [ id "player2" ]
      [
        Html.p [] [ Html.text "Player 2" ],
        Html.p [] [ Html.text ( "game: " ++ ( toString model.gameScore.player2 ) )],
        Html.p [] [ Html.text ( toString model.matchScore.player2 ) ],
        Html.button [ onClick (NewPoint Player2) ] [ Html.text "+" ]
      ]
    ],
    Html.footer []
    [
      Html.button [ onClick Reset ] [ Html.text "Reset" ]
    ]
  ]

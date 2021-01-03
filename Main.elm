module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Browser 

import Dict exposing (Dict)

main = Browser.sandbox { init = initialModel, view = view, update = update }

type Player = X | O

type GameState = Started | Finished (Maybe Player)

-- when the game ended gameState is become Finished <Winner>
-- In case of a tie <Winner> = Nothing

type alias Board = Dict (Int,Int) Player  

-- dictionary board is structured as follows:
-- key -- cell coordinates (x,y); value -- X or O; 
-- Dict stored only used cells

type alias Model =
    { board : Board, currentPlayer : Player, gameState : GameState , boardSize : Int}

initialModel : Model
initialModel =
    { board = Dict.empty, currentPlayer = X, gameState = Started, boardSize = 3 }

type Msg
    = SquareClick (Int,Int) 
    | Reset 
    | AddCell
    | DecCell

changePlayer : Player -> Player
changePlayer player = 
   case player of
       X -> O
       O -> X

update : Msg -> Model -> Model
update msg model =
  case msg of
    SquareClick (x,y) -> 
      if( model.gameState /= Started)
      then
        model
      else
        let 
          newBoard = Dict.insert (x,y) model.currentPlayer model.board
        in
          if(isEmpty model (x,y))
          then
          { 
            model 
            |currentPlayer = (changePlayer model.currentPlayer)
            ,board = newBoard
            , gameState = 
                if(Dict.size model.board < (model.boardSize * model.boardSize - 1) && (checkWinner newBoard model) == Nothing)
                then 
                  Started
                else
                  case checkWinner newBoard model of
                  Just X -> Finished (Just X)
                  Just O -> Finished (Just O)
                  _ -> Finished (Nothing)
          }
          else
            model

    Reset -> initialModel
    AddCell -> 
          let
            newBoardSize = if model.boardSize > 4
                           then 
                              5
                           else
                              model.boardSize + 1
          in 
            {model| boardSize = newBoardSize
                  , board = Dict.empty 
                  , currentPlayer = X 
                  , gameState = Started}  
    DecCell ->
          let 
            newBoardSize = if model.boardSize < 4
                           then 
                              3
                           else
                             model.boardSize - 1
          in 
            {model| boardSize = newBoardSize
                  , board = Dict.empty 
                  , currentPlayer = X 
                  , gameState = Started}

-- next 8 functions are needed for the view 

writePlayer : Player -> String
writePlayer player = 
   case player of
      X -> "player X"
      O -> "player O"

showPlayer : Player -> String
showPlayer player = 
   case player of
      X -> "X"
      O -> "O"

viewStatus : Model -> String
viewStatus model =
   case model.gameState of
      Finished winner ->
         case winner of
            Just player -> 
                "Win " ++ writePlayer player 
            Nothing -> "Draw"   
      Started -> "Turn " ++ writePlayer model.currentPlayer

viewBoard : Model -> List (Html Msg)
viewBoard model = 
    List.range 0 (model.boardSize-1)  
    |> List.map (\x ->
       div classRow ( 
         List.range 0 (model.boardSize-1) 
         |> List.map (\y ->
           div (classSquare ++ [ onClick <| SquareClick (x,y) ]) [
              case Dict.get (x,y) model.board of
                   Just player -> text <| showPlayer player
                   Nothing -> text <| ""
           ] 
         ) 
       )  
    )

classMain =
  [
       style "font-family" "verdana,arial,serif"
     , style "color" "rgb(55,55,55)"
     , style "margin-top" "1%"
     , style "margin-left" "5vw"
     , style "align-items" "center"
  ]

classRow =
  [
       style "display" "flex"
     , style "flex-direction" "row"
     , style "align-items" "center"
     
  ]

classSquare = 
  [
       style "border" "2px solid white"
     , style "width" "100px"
     , style "height" "100px"
     , style "font-size" "80px"
     , style "font-weight" "normal"
     , style "color" "white"
     , style "text-align" "center"
     , style "vertical-aligh" "middle"
     , style "background-color" "grey"
     , style "cursor" "pointer"
     , style "align-items" "center"

  ]

classStatus =
  [
       style "font-size" "30px"
     , style "margin" "10px"
     , style "margin-left" "0px"
  ]
resetStyle = 
  [
      style "margin-top" "10px"
    , style "margin-right" "5px"
    , style "padding" "5px 30px"
    , onClick Reset
  ]

incCell = 
  [
      style "padding" "5px 25px"
    , style "margin-right" "5px"
    , onClick AddCell
  ]
decCell =
  [
      style "padding" "5px 25px"
    , style "margin-right" "5px"
    , onClick DecCell
  ]

view : Model -> Html Msg
view model =
  div classMain [
        div classStatus [ text <| viewStatus model ]
      , div [] <| viewBoard model
      , button resetStyle [ text "Reset" ]
      , button incCell [text "+"]
      , button decCell [text "-"]
   ]      

-- View ends here

-- auxiliary functions for determining the winning position (checkWinner) 

-- returns a list of coordinates coordinates of the given string
row model y =  
   List.range 0 (model.boardSize - 1) 
   |> List.map (\x -> (x,y))
 
-- returns a list of coordinates of the fields of the given column
collumn model x =  
   List.range 0 (model.boardSize - 1) 
   |> List.map (\y -> (x,y)) 

-- returns a list of coordinates of the main diagonal
diagonal1 model = 
   List.range 0 (model.boardSize-1) 
   |> List.map (\i -> (i,i)) 

-- returns a list of coordinates of the side diagonal
diagonal2 model = 
   List.range 0 (model.boardSize-1) 
   |> List.map (\i -> (i,model.boardSize-1-i)) 


winPositions : Model -> List (List (Int,Int)) 
winPositions model =
   (
     List.range 0 (model.boardSize-1) 
     |> List.map (\y -> row model y)
   )
   ++
   (
     List.range 0 (model.boardSize-1) 
     |> List.map (\x -> collumn model x)
   )
   ++
   [diagonal1 model,diagonal2 model]

allEqual : Board -> List (Int,Int) -> Maybe Player
allEqual board l = 
  case l of
    [] -> Nothing
    h::t -> if 
              List.all (\x -> Dict.get h board == Dict.get x board) t 
            then 
              Dict.get h board
            else 
              Nothing


isEmpty :Model -> (Int,Int) -> Bool
isEmpty model (x,y) =
  case Dict.get (x,y) model.board of
    Just _ -> False
    Nothing -> True


checkWinner: Board -> Model -> Maybe Player
-- the result can be Just X, Just O if the winner is X or O
-- if Nothing the game continues
checkWinner board model = 
    Maybe.withDefault Nothing (List.head <| 
                              (List.filter (\x->case x of
                                                Just _ -> True
                                                _ -> False) 
                                           (List.map (allEqual board) (winPositions model))))

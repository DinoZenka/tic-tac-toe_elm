module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Browser 

import Dict exposing (Dict)

main = Browser.sandbox { init = initialModel, view = view, update = update }

boardSize = 3

type Player = X | O

type GameState = Started | Finished (Maybe Player)

-- when the game ended gameState is become Finished <Winner>
-- In case of a tie <Winner> = Nothing

type alias Board = Dict (Int,Int) Player  

-- dictionary board is structured as follows:
-- key -- cell coordinates (x,y); value -- X or O; 
-- Dict stored only used cells

type alias Model =
    { board : Board, currentPlayer : Player, gameState : GameState }

initialModel : Model
initialModel =
    { board = Dict.empty, currentPlayer = X, gameState = Started }

type Msg
    = SquareClick (Int,Int)

changePlayer : Player -> Player
changePlayer player = 
   case player of
       X -> O
       O -> X

update : Msg -> Model -> Model
update msg model =
  case msg of
    SquareClick (x,y) ->
       { model | board = Dict.insert (x,y) model.currentPlayer model.board }

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
    List.range 0 (boardSize-1)  
    |> List.map (\x ->
       div classRow ( 
         List.range 0 (boardSize-1) 
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
  ]

classRow =
  [
       style "display" "flex"
     , style "flex-direction" "row"
  ]

classSquare = 
  [
       style "border" "1px solid rgb(125,125,125)"
     , style "width" "70px"
     , style "height" "70px"
     , style "margin-right" "-1px"
     , style "margin-top" "-1px"
     , style "font-size" "55px"
     , style "font-weight" "bold"
     , style "text-align" "center"
       
  ]

classStatus =
  [
       style "font-size" "20px"
     , style "margin" "10px"
     , style "margin-left" "0px"
  ]


view : Model -> Html Msg
view model =
  div classMain [
        div classStatus [ text <| viewStatus model ]
      , div [] <| viewBoard model
   ]      

-- View ends here

-- auxiliary functions for determining the winning position (checkWinner) 

-- returns a list of coordinates coordinates of the given string
row y =  
   List.range 0 (boardSize-1) 
   |> List.map (\x -> (x,y))
 
-- returns a list of coordinates of the fields of the given column
collumn x =  
   List.range 0 (boardSize-1) 
   |> List.map (\y -> (x,y)) 

-- returns a list of coordinates of the main diagonal
diagonal1 = 
   List.range 0 (boardSize-1) 
   |> List.map (\i -> (i,i)) 

-- returns a list of coordinates of the side diagonal
diagonal2 = 
   List.range 0 (boardSize-1) 
   |> List.map (\i -> (i,boardSize-1-i)) 

winPositions : List (List (Int,Int)) 
winPositions =
   (
     List.range 0 (boardSize-1) 
     |> List.map (\y -> row y)
   )
   ++
   (
     List.range 0 (boardSize-1) 
     |> List.map (\x -> collumn x)
   )
   ++
   [diagonal1,diagonal2]

-- checks that all the elements of the list are the same and returns this value
-- if different - returns Nothing

allEqual : List a -> Maybe a
allEqual l = 
  case l of
    [] -> Nothing
    h::t -> if List.all (\x -> x == h) t then Just h else Nothing

maybeJoin : Maybe (Maybe a) -> Maybe a
maybeJoin mm = 
  case mm of
    Nothing -> Nothing
    Just m -> m

something : Maybe a -> Bool
something m = 
  case m of
    Just _ -> True
    Nothing -> False  

--checkWinner: Board -> Maybe Player
-- the result can be Just X, Just O if the winner is X or O
-- if Nothing the game continues
--checkWinner board = 
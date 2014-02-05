-- vim: syntax=haskell:

import Window
import Mouse
import Dict

-- Input

type Position = {x:Int, y:Int}

type Input = { position : Position }

clock = inSeconds <~ every second

input : Input
input = sampleOn (merge (lift (\_ -> () ) clock) Mouse.clicks) ( Input <~ Mouse.position )

-- Model

type Button = { onClick : Game -> Game, position : Position, size : Position, text : String }
data State = Paused | Playing
type Board = { size : Position, units : Dict.Dict Position () }
type Game = { buttons : [ Button ], state : State, board : Board }

type Constants = { size : Position, tileSize : Position }
constants : Constants
constants = Constants defaultSize defaultTileSize
defaultTileSize = (20,20)

-- Update

updateBoard : Board -> Board
updateBoard ({units} as board) = board

updateGame : Game -> Game
updateGame ({state, board} as game) = case state of
 Paused -> game
 Playing -> {game | board <- updateBoard board}

buttonClick : Position -> Button -> Bool
buttonClick {x,y} {position, size} = x >= position.x && x < position.x + size.x && y >= position.y && y < position.y + size.y 

buttonHandleClick : [Button] -> Game -> Game
buttonHandleClick s = case s of
  [] -> id
  ({onClick}::_) -> onClick

handleClickAt : Position -> Game -> Game
handleClickAt pos ({buttons} as game) = let bs = filter (buttonClick pos) buttons in buttonHandleClick bs game

stepGame : Input -> Game -> Game
stepGame { delta, position } = if Mouse.isClicked then handleClickAt position else updateGame

gameState = foldp stepGame input defaultGame

defaultGame = Game defaultButtons defaultState defaultBoard

defaultSize = (40,40)
defaultButtons = []
defaultState = Paused
defaultBoard = Board constants.size Dict.empty

-- Display

displayState state = if state == Playing then empty else asText state

placeTile {x,y} = move (x*constants.tileSize.x,y*constants.tileSize.y) 

circleAt {x,y} = oval constants.tileSize.x constants.tileSize.y |> filled white |> placeTile (x,y)

crossAt {x,y} = rect constants.tileSize.x constants.tileSize.y |> outlined defaultLine |> placeTile (x,y)

displayTile d p = if p `Dict.lookup` d /= Nothing then circleAt p else crossAt p

allPairs {x,y} = map (\a ->  map (\b -> (a,b)) [0..x-1] ) [0..y-1]

displayBoard ({dict} as board) = layers <| map (displayTile dict) <| allPairs board.size.x board.size.y

display (w,h) {buttons, board, state} = container w h middle <| collage constants.size.x constants.size.y [
  displayState state,
  displayBoard board
  ] ++ map displayTile buttons

main = display <~ Window.dimensions ~ gameState
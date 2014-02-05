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
type Board = { size : Position, units : Dict Position () }
type Game = { buttons : [ Button ], state : State, board : Board }

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
buttonHandleClick [] = id
buttonHandleClick ({onClick}::_) = onClick

handleClickAt : Position -> Game -> Game
handleClickAt pos ({buttons} as game) = let bs = filter (buttonClick pos) buttons in buttonHandleClick bs game

stepGame : Input -> Game -> Game
stepGame { delta, position } = if Mouse.isClicked then handleClickAt position else updateGame

gameState = foldp stepGame input defaultGame

defaultGame = Game defaultButtons defaultState defaultBoard

defaultButtons = []
defaultState = Paused
defaultBoard = Board constants.size Dict.empty

-- Display
display (w,h) {buttons, board, state} 
main = display <~ Windows.dimensions ~ gameState

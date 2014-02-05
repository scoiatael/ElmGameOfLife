-- vim: syntax=haskell:
import Window
import Mouse
import Dict
import List

-- Input

type Position = {x:Int, y:Int}

type Comp = (Int, Int)

toComp : Position -> Comp
toComp {x,y} = (x,y)

fromTuple : (Int,Int) -> Position
fromTuple (x,y) = {x = x, y = y}

type Input = { position : Position }

clock = inSeconds <~ every second

input = sampleOn (clock) ( Input . fromTuple <~ Mouse.position )


-- Model

type Button = { onClick : Game -> Game, position : Position, size : Position, text : String }
data State = Paused | Playing
type Board = { size : Position, units : Dict.Dict Comp () }
data Game = Game { buttons : [ Button ], state : State, board : Board }

type Constants = { size : Position, tileSize : Position, winSize : Position, scale : Float }
constants : Constants
constants = let (tx,ty) = toComp defaultTileSize in let (x,y) = toComp defaultSize in Constants defaultSize defaultTileSize ( fromTuple (tx*x, ty*y)) defaultScale
defaultScale = 0.8
defaultTileSize = {x = 20, y = 20}
defaultSize = {x = 40, y = 40}

-- Update

updateBoard : Board -> Board
updateBoard ({units} as board) = board

updateGame : Game -> Game
updateGame (Game ({state,board} as game)) = case state of
  Paused -> Game game
  Playing -> Game {game | board <- board |> updateBoard}

buttonClick : Position -> Button -> Bool
buttonClick {x,y} {position, size} = (x >= position.x) && (x < position.x + size.x) &&( y >= position.y) && (y < position.y + size.y)

buttonHandleClick : [Button] -> Game -> Game
buttonHandleClick s = case s of
  [] -> id
  ({onClick}::_) -> onClick

handleClickAt : Position -> Game -> Game
handleClickAt pos ((Game {buttons}) as game) = let bs = filter (buttonClick pos) <| buttons in buttonHandleClick bs game

stepGame : Input -> Game -> Game
stepGame { position } = updateGame

gameState : Signal Game
gameState = foldp stepGame defaultGame input 

defaultGame : Game
defaultGame = Game { buttons = defaultButtons, state =  defaultState, board =  defaultBoard}

defaultButtons : [ Button ]
defaultButtons = [ unpauseButton ]

defaultState : State
defaultState = Paused

defaultBoard : Board
defaultBoard = { size = constants.size, units =  Dict.empty}

unpauseButton = { onClick = \(Game g) -> Game {g | state <- Playing }, position = fromTuple (-20,-20), size = fromTuple (100, 30), text = "Unpause" }

-- Display

displayState : State -> Form
displayState state = scale 2 <| toForm <| if state == Playing then empty else asText state

placeTile : Comp -> Form -> Form
placeTile (x,y) = move (toFloat <| x * constants.tileSize.x - constants.winSize.x `div` 2, toFloat <| y * constants.tileSize.y - constants.winSize.y `div` 2) 

circleAt : Comp -> Form
circleAt ( (x,y) as coords) = toFloat constants.tileSize.x `oval` toFloat constants.tileSize.y |> filled white |> placeTile coords

crossAt : Comp -> Form
crossAt ( (x,y) as coords) = toFloat constants.tileSize.x `rect` toFloat constants.tileSize.y |> outlined defaultLine |> placeTile coords

displayTile : Dict.Dict Comp () -> Comp -> Form
displayTile d p = if p `Dict.lookup` d /= Nothing then circleAt p else crossAt p

allPairs : Position -> [Comp]
allPairs {x,y} = List.concat <| map (\a -> (map (\b -> (a,b)) [0..x-1] )) [0..y-1]

displayBoard : Board -> Form
displayBoard ({units} as board) = group <| map (displayTile units) <| allPairs <| fromTuple (board.size.x,board.size.y)

buttonGreen = rgb 60 100 60

displayButton : Button -> Form
displayButton {position, size, text} = move (toFloat position.x, toFloat position.y) <| group [ 
  filled buttonGreen <| toFloat size.x `rect` toFloat size.y,
  toForm <| plainText text ]

scaleElement w h el = let fw = toFloat w in let fh = toFloat h in collage w h [ 
  scale ( (fw / toFloat (widthOf el) * constants.scale) `min` (fh / toFloat (heightOf el) * constants.scale) ) <| toForm el ]

display (w,h) (Game {buttons, board, state}) = layers [
  ( container w h middle <| scaleElement w h <| collage (constants.winSize.x + constants.tileSize.x) (constants.winSize.y + constants.tileSize.y) <| [ displayBoard board ] ),
  ( container w h middle <| collage 100 100  <| [ move (0 - toFloat w / 4, 0 - toFloat h / 4) <| group <| [ displayState state ] ++ map displayButton buttons ] )
 ]

main = display <~ Window.dimensions ~ gameState

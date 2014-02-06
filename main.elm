-- vim: syntax=haskell:
import Window
import Mouse
import Dict
import List
import Graphics.Input as Input

-- Input

type Position = {x:Int, y:Int}

type Comp = (Int, Int)

toComp : Position -> Comp
toComp {x,y} = (x,y)

fromTuple : (Int,Int) -> Position
fromTuple (x,y) = {x = x, y = y}

data Input = Time { delta : Float } | Click { comp : Comp } | 
  Checkbox { isChecked : Bool, element : Element } | DropDown { name : String, element : Element }
  | Redraw { void : () }
clock = inSeconds <~ fps 25

type MutableElement a = { update : Signal a, element : Signal Element} 
fromInput : (Signal Element, Signal a) -> MutableElement a
fromInput (sigel, siga) = { update = siga, element = sigel }

checkBox : MutableElement Bool
checkBox = fromInput <| Input.checkbox True

dropDown : MutableElement String
dropDown = fromInput <| Input.stringDropDown ["1","2","3","4","8","16","32", "64", "128", "256"]

dot2 : (a -> b) -> (c -> d -> a) -> c -> d -> b
dot2 f a x y = f (a x y)

tileButtons = Input.customButtons (-1,-1)

input = combine <| List.reverse [ Time . (\v -> { delta = v} )        <~ clock,  
  (Click . (\c -> { comp = c }))                                      <~ (dropRepeats <| dropIf ((==) (-1,-1)) (-2,-2) tileButtons.events),
  (dot2 Checkbox  (\b e -> { isChecked = b, element = e } ) )         <~ checkBox.update ~ checkBox.element, 
  (Redraw . (\_ -> {void = ()}))                                      <~ Window.dimensions,
  (dot2 DropDown  (\d e -> { name = d,  element = e } )     )         <~ dropDown.update ~ dropDown.element ] 


-- Model

defaultScale = 0.95
defaultTileSize = {x = 20, y = 20}
defaultSize = {x = 40, y = 40}
defaultOffset = 40

makeTile : Color -> Element
makeTile color = collage defaultTileSize.x defaultTileSize.y [ toFloat defaultTileSize.x `oval` toFloat defaultTileSize.y |> filled color ]

alphaRed = rgba 200 100 100 128
noColor = rgba 0 0 0 0 
clearRed = rgba 200 100 100 255

defaultUp =  makeTile noColor
defaultDown =  makeTile clearRed
defaultHover =  makeTile alphaRed

type Constants = { size : Position, tileSize : Position, winSize : Position, scale : Float, offset : Int, tileButtonUp : Element, tileButtonDown : Element, tileButtonHover : Element }
constants : Constants
constants = let (tx,ty) = toComp defaultTileSize in let (x,y) = toComp defaultSize in 
  Constants defaultSize defaultTileSize ( fromTuple ( tx * (x+1), ty * (y+1) ) ) defaultScale defaultOffset defaultUp defaultDown defaultHover

data State = Paused | Playing
type Board = { size : Position, units : Dict.Dict Comp () }
data Game = Game { dropDown : Element, checkBox : Element, 
  state : State, board : Board, speed : Int, 
  timeDelta : Float, clicks : [Comp], redraw:Bool}

-- Update
repeat : Int -> a -> (a -> a) -> a
repeat i a f = case i of
  0 -> a
  n -> repeat (i-1) (f a) f

neighbours : Int -> Int -> [Comp]
neighbours x y = [(x-1,y), (x+1,y), (x,y-1), (x,y+1), (x-1,y-1), (x+1,y+1), (x+1,y-1), (x-1,y+1)]

aliveCell : Comp -> Board -> Bool
aliveCell c {units} = c `Dict.lookup` units  /= Nothing

countNeighbours : Board -> Int -> Int -> Int
countNeighbours b x y = foldl (+) 0 <| map (\c -> if aliveCell c b then 1 else 0) <| neighbours x y

type CellChange = Comp -> Board -> Board

onUnits : Board -> (Dict.Dict Comp () -> Dict.Dict Comp ()) -> Board
onUnits ({units} as b) f = { b | units <- f units}

killCell : CellChange
killCell c b = onUnits b <| Dict.remove c

prolongCell : CellChange
prolongCell c b = b

reviveCell : CellChange
reviveCell c b = onUnits b <| Dict.insert c ()

processCell : Comp -> Int -> Board -> Board
processCell coords neigh board = case neigh of 
  0 -> killCell coords board
  1 -> killCell coords board
  2 -> prolongCell coords board
  3 -> reviveCell coords board
  i -> killCell coords board

updateBoard : Board -> Board
updateBoard ({units, size} as board) = foldl (\(x,y,c) -> processCell (x,y) c) board <| 
  map (\(x,y) -> (x,y, countNeighbours board x y)) <| allPairs size

updateGame : Float -> Game -> Game
updateGame delta (Game ({state, board, speed, timeDelta} as game)) = case state of
  Paused -> Game game
  Playing -> let dt = 1 / toFloat speed in Game <| if timeDelta > dt then { game | timeDelta <- timeDelta + delta - dt, 
                                                                               -- redraw <- True,
                                                                                board <- updateBoard board}
                                                                  else { game | timeDelta <- timeDelta + delta }

fromMaybeWithDefault : a -> Maybe a -> a
fromMaybeWithDefault a ma = case ma of
  Nothing -> a
  Just i  -> i
 
click : Comp -> Game -> Game
click cords (Game ({board} as g)) = Game {g | board <- if aliveCell cords board then killCell cords board else reviveCell cords board} 

inside (min, max) i = i >= min && i < max

--insideBoard (x,y) = inside (0, constants.size.x) x && inside (0, constants.size.y) y 
insideBoard (x,y) = (x /= -2) && (y /= -2)

addClick comp (Game ({clicks} as g)) = Game {g | clicks <- comp :: take 29 clicks}

handleClick comp (Game ({clicks} as g)) = (if insideBoard comp && (clicks == [] || comp /= head clicks) 
  then -- setRedraw True <| 
    click comp <| addClick comp <| Game g 
  else Game g ) 

setRedraw : Bool -> Game -> Game
setRedraw b (Game g) = Game { g | redraw <- b }

stepGame : Input -> Game -> Game
stepGame inp g = setRedraw False g |>  case inp of
  Time { delta }                  -> updateGame delta 
  Click { comp }                  -> handleClick comp
  Checkbox { isChecked, element}  -> (\(Game g) -> Game { g | checkBox <- element, 
                                                              state <- if isChecked then Paused else Playing 
                                                              } ) -- . setRedraw True
  DropDown { name, element }      -> (\(Game g) -> Game { g | dropDown <- element, 
                                                              speed <- fromMaybeWithDefault 1 <| String.toInt name
                                                              } ) -- . setRedraw True
  Redraw { void }                 -> setRedraw True

stepGameList : [Input] -> Game -> Game
stepGameList ins game = foldl stepGame game ins
  
gameState : Signal Game
gameState = foldp stepGameList defaultGame input 

defaultGame : Game
defaultGame = Game { state =  defaultState, board =  defaultBoard, 
                      checkBox = empty, dropDown = empty, speed = 1, 
                        timeDelta = 0, clicks = [], redraw = True }

defaultState : State
defaultState = Paused

appl2 : (a -> () -> c) -> a -> c
appl2 f a = f a ()

fromStr : String -> [Comp]
fromStr s = concatMap (\(a,l) -> map (\x -> (x,a)) <| String.indexes "1" l ) <| zip [0..constants.size.y] <| String.lines s

defaultBoard : Board
defaultBoard = { size = constants.size, units = foldl (appl2 Dict.insert) Dict.empty <|
  map (\(a,b) -> (a {- + constants.size.x `div` 3 - 7-}, b {- + constants.size.y `div` 3 + 5-})) <| 
    fromStr "0000000
             0111010
             0100000
             0000110
             0011010
             0101010
             0000000" }

-- Display

displayState : State -> Form
displayState state = scale 8 <| toForm <| if state == Playing then empty else asText state

placeTile : Comp -> Form -> Form
placeTile (x,y) = move (toFloat <| (x+1) * constants.tileSize.x - constants.winSize.x `div` 2, toFloat <| (y+1) * constants.tileSize.y - constants.winSize.y `div` 2) 

circleAt : Comp -> Form
circleAt ( (x,y) as coords) = toFloat constants.tileSize.x `oval` toFloat constants.tileSize.y |> filled white 
crossAt : Comp -> Form
crossAt ( (x,y) as coords) = toFloat constants.tileSize.x `rect` toFloat constants.tileSize.y |> outlined defaultLine

displayTile : Dict.Dict Comp () -> Comp -> Form
displayTile d p = (if p `Dict.lookup` d /= Nothing then circleAt p else crossAt p) |> placeTile p

displayButton p = placeTile p <| toForm <| tileButtons.customButton p constants.tileButtonUp constants.tileButtonHover constants.tileButtonDown

allPairs : Position -> [Comp]
allPairs {x,y} = List.concat <| map (\a -> (map (\b -> (a,b)) [0..x-1] )) [0..y-1]

displayBoard : Board -> Form
displayBoard ({units} as board) = group <| map (displayTile units) <| allPairs <| fromTuple (constants.size.x,constants.size.y)

displayButtons : Form
displayButtons = group <| map displayButton <| allPairs <| fromTuple (constants.size.x,constants.size.y)

scaleElement w h el = let fw = toFloat w in let fh = toFloat h in collage w h [ 
  scale (boxSize fw fh (toFloat (widthOf el)) (toFloat (heightOf el))) <| toForm el ]

times : ((a->b), (c->d)) -> (a,c) -> (b,d)
times (f,g) (a,b) = (f a, g b)

unScale : Comp -> Comp -> Comp
-- Window dimensions -> Clicked position -> Board coords
unScale (w,h) (cx, cy) = let cy' = cy + constants.offset
                             scale = boxSize (toFloat w) (toFloat h) (toFloat constants.winSize.x) (toFloat constants.winSize.y)  in
  times (\x -> x - constants.size.x `div` 2, \y -> constants.size.y -  y) <|  
    (floor <| toFloat (cx-3*constants.tileSize.x + 2) / scale / toFloat constants.tileSize.x, 
      floor <| toFloat (cy'-4*constants.tileSize.y) / scale / toFloat constants.tileSize.y)

boxSize fw fh tw th =  (fw / tw * constants.scale) `min` (fh / th * constants.scale) 

bkgColour = rgb 200 200 200

background w h = collage w h [ filled bkgColour <| toFloat w `rect` toFloat h ]

display (w,h) (Game {board, state, dropDown, checkBox, clicks, redraw}) = -- if not redraw then empty else 
  let h' = h - constants.offset 
      w' = w - 60 in layers 
    [background w h, container w h middle <| collage w h [ displayState state ],
      flow down <| List.reverse [
      ( beside ( above (plainText "Clicks:") <| width 60 <| asText <| take 30 clicks) <| 
          container w' h' middle <| scaleElement w' h' <| collage constants.winSize.x constants.winSize.y <| 
        [ displayBoard board , if redraw then 
                                          displayButtons
                                         else toForm empty
                                          ] ),
      ( container w (constants.offset `div` 2) middle <| flow left <| intersperse (spacer 60 10) 
        [ plainText "Speed: ", width 60 dropDown, plainText "Pause: " , height 20 checkBox ] ),
      spacer 10 (constants.offset `div` 2)
     ]]

main = display <~ Window.dimensions ~ gameState 

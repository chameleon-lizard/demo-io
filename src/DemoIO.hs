module DemoIO where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

--------------
-- Data types.
--------------

-- Config for colors.
data ColorConfig = ColorConfig
  { color1 :: Color,
    color2 :: Color
  }

type Position = (Int, Int)

data Direction = U | D | L | R

type Map = [(Tile, Position)]

data Tile = Player | Box | Wall | Storage | Free

-- General application state.
data AppState = AppState
  { position :: Position,
    direction :: Direction,
    map :: Map,
    isFinished :: Bool
  }

-------------
-- Constants.
-------------

-- Path to config file.
configPath :: FilePath
configPath = "config.txt"

-- Random numbers range.
range :: (Int, Int)
range = (-10, 10)

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = black

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 200

------------------
-- Pure functions.
------------------

isPassable :: AppState -> Bool
isPassable (pos, dir, map, finished)
  | res == Free or Storage = True
  | res == Box = canMoveBox (pos, dir, map, finished)
  | otherwise = False
  where
    res = maze (fst pos + x, snd pos + y)
    x
      | dir == U = 0
      | dir == D = 0
      | dir == L = - 1
      | otherwise = 1
    y
      | dir == L = 0
      | dir == R = 0
      | dir == D = - 1
      | otherwise = 1

canMoveBox :: AppState -> Bool
canMoveBox (pos, dir, map, finished)
  | res == Free or Storage = True
  | otherwise = False
  where
    res = maze (fst pos + x, snd pos + y)
    x
      | dir == U = 0
      | dir == D = 0
      | dir == L = - 2
      | otherwise = 2
    y
      | dir == L = 0
      | dir == R = 0
      | dir == D = - 2
      | otherwise = 2

-- Draw a picture: two numbers of different colors defined in config.
drawApp :: AppState -> Picture
drawApp (AppState (ColorConfig c1 c2) n _) = Pictures [pic1, pic2]
  where
    txt = Text (show n)
    pic1 = Translate (- textShift) 0 $ Color c1 txt
    pic2 = Translate textShift 0 $ Color c2 txt

-- Handle events.
handleEvent :: Event -> AppState -> AppState
-- Go up when UP is pressed.
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
  state {position = if isPassable state then (fst position, snd position + 1) else position}
-- Go down when DOWN is pressed.
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
  state {position = if isPassable state then (fst position, snd position - 1) else position}
-- Go left when LEFT is pressed.
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {position = if isPassable state then (fst position - 1, snd position) else position}
-- Go right when RIGHT is pressed.
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {position = if isPassable state then (fst position + 1, snd position) else position}
-- Ignore all other events.
handleEvent _ state = state

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

------------------------------
-- Main function for this app.
------------------------------

tileSize :: Int
tileSize = 32

prepareData :: [String] -> Map
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0..length rawData -1]]

makeRow :: String -> Int -> Map
makeRow row y =
 [ ( ( (fromIntegral x * tileSize) - ((1024 - tileSize) / 2)
 , (fromIntegral y * tileSize) - ((768 - tileSize) / 2))
 , row !! x)
 | x <- [0 .. length row - 1]
 , row !! x == '*' || row !! x == '%' || row !! x == '.'
 ]



initState :: AppState
initState = AppState {position = (0, 0), direction = U, _, isFinished = False}

-- General application state.
data AppState = AppState
  { position :: Position,
    direction :: Direction,
    map :: Map,
    isFinished :: Bool
  }

run :: IO ()
run = do
  let initState = AppState cfg 0 gen
  play display bgColor fps initState drawApp handleEvent updateApp

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

-- Returns the tile type from coordinate
maze :: Position -> Map -> Tile
maze pos = snd head filter (\x -> fst x == pos)

-- Draw tiles
drawTileAt :: Position -> Map -> Picture
drawSplash :: Picture
getTile :: Tile -> Picture
drawMap :: State -> Picture
-- Interactions
handleEvent :: Event -> State -> State
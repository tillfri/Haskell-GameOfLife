import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

-- |representing a cell in the game grid
data CellState = Dead | Alive deriving (Show , Eq )

-- |representing a cell position in the game grid
data Position = Position {x :: Int, y :: Int} deriving Show 

-- |representing a GameOfLife, grid contains the game logic, time saves time since last render call, overlay holds lines for the gui, width and height determine the size of the gui display,
-- isRunning start/stops game, originState keeps the original state for Event 'r', fps determines the frames/iterations per second and generation is a counter to show the amount of iterations
data Game = Game {grid :: [[CellState]],
                  time :: Float,
                  overlay :: [Picture],
                  windowWidth :: Int,
                  windowHeight :: Int,
                  isRunning :: Bool,
                  originState :: [[CellState]],
                  fps :: Int,
                  generation :: Int} deriving Show 


-- |converts a [[CellState]] into a Game
createGame :: [[CellState]] -> Game
createGame grid = Game {grid = grid, 
                        time = 0.0, 
                        windowWidth = float2Int dimension*length (head grid), 
                        windowHeight = float2Int dimension*length grid,
                        overlay = createOverlay (length (head grid)) (length grid) (float2Int dimension*length (head grid)) (float2Int dimension*length grid),
                        isRunning = False, 
                        originState = grid,
                        fps = 5,
                        generation = 0}
                        

-- |takes a Position and a game grid and returns the CellState of that position in the game grid
getCell :: Position -> [[CellState]] -> CellState
getCell p grid = grid !! y p !! x p 

-- |takes a position and grid lengths and returns the neighbor positions, if neighbor is out of bounds, adjacentPosition will calculate the correct position at the opposite field of the game grid
checkNeighbors :: Position -> Int -> Int -> [Position] 
checkNeighbors (Position x y) ly lx = [adjacentPosition (x-1) (y-1) ly lx,
                                       adjacentPosition (x-1) y ly lx,
                                       adjacentPosition (x-1) (y+1) ly lx, 
                                       adjacentPosition x (y-1) ly lx, 
                                       adjacentPosition x (y+1) ly lx, 
                                       adjacentPosition (x+1) (y-1) ly lx, 
                                       adjacentPosition (x+1) y ly lx, 
                                       adjacentPosition (x+1) (y+1) ly lx]  

-- |takes the x and y coordinates of a position in the game grid aswell as xLength and yLength of the game grid. calculates the correct position in case that the actual position is out of bounds of the game grid, otherwise returns the original position given into the function
adjacentPosition :: Int -> Int -> Int -> Int -> Position
adjacentPosition x y ly lx 
            | inboundsY && inboundsX = Position x y
            | x < 0 && y < 0 = Position lx ly
            | x < 0 && inboundsY = Position lx y
            | x < 0 && y > ly = Position lx 0
            | inboundsX && y < 0 = Position x ly
            | inboundsX && y > ly = Position x 0
            | x > lx && y < 0 = Position 0 ly
            | x > lx && inboundsY = Position 0 y
            | x > lx && y > ly = Position 0 0 
            where
                inboundsX = x >= 0 && x <= lx
                inboundsY = y >= 0 && y <= ly
               
        
-- |takes a [Position] and a game grid and calculates the cellstate for every position. return the amount of living adjacent cells
aliveNeighbors :: [Position] -> [[CellState]] -> Int 
aliveNeighbors [] grid = 0
aliveNeighbors (z:zs) grid = if getCell z grid == Alive then
                                1 + aliveNeighbors zs grid
                                else
                                0 + aliveNeighbors zs grid

-- |return the new CellState depending on the previous CellState and the amount of living neighbors
decideCellFate :: CellState -> Int -> CellState
decideCellFate state neighbors 
         | state == Dead && neighbors == 3 = Alive
         | state == Alive && (neighbors == 2 || neighbors == 3) = Alive
         | otherwise = Dead

-- |takes a Game and returns the adjusted Game with the new generation in game grid
nextGen :: Game -> Game
nextGen game = game { grid = newGeneration (grid game)} 

-- |helper for nextGen, computes the next iteration of CellState for each row of a game grid
newGeneration :: [[CellState]] -> [[CellState]]
newGeneration generation = computeRow generation 0
                    where 
                        computeRow g y
                            | y == length g = []
                            | y < length g = [state | x <- [0..(length (head g)-1)],
                                let state = decideCellFate (getCell (Position x y) g) (aliveNeighbors (checkNeighbors (Position x y) (length g -1) (length (head g)-1)) g) ] : computeRow g (y+1)
                                
-- IO Section

-- |read a text file and convert it into a game grid
-- Rules:
    --dead cells are represented by the character 'O', alive cells by the character 'X'
    --the range/length of column and row is 0 < x < maxInt
        --but the first column and each column after that need to be the same length, same goes for row
    --note that the string function "lines" has to be applied to the original string read from the file, therefore linebreaks have to be placed carefully (don't press enter after your last column)
-- Example: str <- readFile "starts/pulsGlider"
--          let game = createGame $ gameFromFile $ lines str
readGameFromFile :: [String] -> [[CellState]]
readGameFromFile [] = []
readGameFromFile xs = map
                        (map
                            (\ y
                                -> if y == 'O' then
                                    Dead
                                else
                                    if y == 'X' then
                                        Alive
                                    else
                                        error "translation failed due to illegal char"))
                        xs

-- |a composition of lines,readGameFromFile and createGame to create a game from a textfile (textfile has to meet certain criteria, for more details check readGameFromFile description)
createGameFromFile :: String -> Game
createGameFromFile str = createGame $ readGameFromFile $ lines str        

-- GUI Section

-- |size in pixel for the rectangle/cell, entire GUI (window size, grid size) will adjust to this value
-- |recommended values: 10 < x < 40
dimension :: Float 
dimension = 20

-- |backgroundcolor of the window
background :: Color
background = white

-- |color for dead cells
colorDeadCells :: Color
colorDeadCells = white 

-- |color for alive cells
colorAliveCells :: Color 
colorAliveCells = black   

-- |changes frames/simulation steps per second, allowed value 0 < x < 20, for x >= 20 documentation of gloss framework recommends compiling the programm with -threaded due to high single-core cpu usage

changeFps :: Int -> Int -> Int
changeFps current change 
                    | current + change >= 20 = 20
                    | current + change <= 1 = 1
                    | otherwise = current + change

-- |creates a window for the gui to run in with the correct size for the game configuration
createWindow :: Game -> Display 
createWindow game = InWindow "Conways Game of Life" (windowWidth game + float2Int dimension, windowHeight game + float2Int dimension) (600, 100)

-- |creates the grey gridoverlay depending on the size of the game grid
createOverlay :: Int -> Int -> Int -> Int -> [Picture]
createOverlay xl yl width height = [color (greyN 0.5) li | x <- [0..xl], 
            let p1 = (int2Float x * dimension - int2Float width/2 - dimension/2, int2Float height/2 + dimension/2),
            let p2 = (int2Float x * dimension - int2Float width/2 - dimension/2, int2Float height/2 - (int2Float yl*dimension)+ dimension/2) 
                li = line [p1,p2]] ++ xlist 
                    where
                        xlist = [color (greyN 0.5) lin | y <- [0..yl],
                         let p3 = (- int2Float width/2 - dimension/2, int2Float height/2 - int2Float y*dimension + dimension/2),
                         let p4 = (-int2Float width/2 + int2Float xl*dimension - dimension/2,int2Float height/2 - int2Float y*dimension + dimension/2)
                             lin = line[p3,p4]]


-- |used to create a GUI picture from a grid/gamestate
drawMyGame :: Game -> Picture
drawMyGame game = pictures $ convertGame game ++ overlay game ++ [infoText game]

-- |creates the infotext at the bottom of the window
-- inspired by https://github.com/alexbooth/Game-of-Life-Haskell
infoText :: Game -> Picture
infoText game = translate (int2Float  (-windowWidth game)/2) (int2Float (-windowHeight game)/2-10) 
                $ scale 0.15 0.15 
                $ text ("FPS: " ++ show (fps game) ++ " Generation: " ++ show (generation game))

-- |translates each cell into a picture with calculated pixelcoordinates depending on their position within the game grid, currently each cell is represented by a white or black solid rectangle, color depending on colorAliveCells/colorDeadCells
convertGame :: Game -> [Picture]
convertGame game = [translate (int2Float x * dimension - int2Float (windowWidth game)/2) (int2Float (windowHeight game)/2 - (int2Float y*dimension) ) pic |
   
   y <- [0 .. length (grid game) -1],
   x <- [0 .. length (head (grid game))-1],
   let c = if getCell (Position x y) (grid game) == Alive then
               colorAliveCells 
           else
               colorDeadCells  
       pic = color c (rectangleSolid dimension dimension)]

-- |translates mouse click coordinates into the correct grid values and calls changeCellState to change the current cellstate from Dead->Alive or Alive->Dead
translateMouse :: Float -> Float -> Game -> [[CellState]]
translateMouse x y game = if xInt >= 0 && xInt < length (head (grid game)) && yInt >= 0 && yInt < length (grid game)
                            then changeCellState (Position xInt yInt) (grid game)
                            else grid game
                                where
                                    xInt = round  ((2*x + int2Float (windowWidth game))/ (2*dimension)) 
                                    yInt = round  ((int2Float (windowHeight game) - 2*y)/(2*dimension))

-- |checks the lifestate of a cell at a certain position, if the cell is alive, insertNewCellState is called with Dead, otherwise inserNewCellState is called with Alive
changeCellState :: Position -> [[CellState]] -> [[CellState]]
changeCellState pos grid = let cell = getCell pos grid
                            in if cell == Alive
                                 then insertNewCellState pos grid Dead
                                 else insertNewCellState pos grid Alive

-- |performs a combination of splitAt operations to isolate and replace a cell at a certain position with a given state
insertNewCellState :: Position -> [[CellState]] -> CellState -> [[CellState]]
insertNewCellState pos grid state = let firstSplit = splitAt (y pos) grid 
                                        secondSplit = splitAt 1 $ snd firstSplit 
                                        thirdSplit = splitAt (x pos) $ head (fst secondSplit) 
                                        replaceCell = state : tail (snd thirdSplit) 
                                        firstBack = fst thirdSplit ++ replaceCell
                                        secondBack = firstBack : snd secondSplit
                                        thirdBack = fst firstSplit ++ secondBack
                                        in thirdBack                     

                             
-- |deprecated, might be of interest for presentation/documention                   
renderNewGen :: Float -> Game -> Game
renderNewGen seconds game = let state = grid game in
                            createGame $ iterations state (float2Int seconds)
                            where
                                iterations ga 0 = ga
                                iterations ga sec = iterations (newGeneration ga) (sec-1)

-- |takes time since last function call and a game and returns a game. renders new gamestate if enough time has passed (depends on fps :: Int), otherwise keeps the current gamestate
-- function taken from https://github.com/alexbooth/Game-of-Life-Haskell (update)
render :: Float -> Game -> Game
render deltatime game 
                    | isRunning game && time game >= 1/fromIntegral (fps game) = game {grid = newGeneration (grid game),time = time game + deltatime - (1/fromIntegral (fps game)), generation = generation game +1}
                    | isRunning game = game { time = time game + deltatime }
                    | otherwise = game

-- Testing (deprecated)
blinker :: [[CellState]]
blinker = [[Dead,Dead,Dead,Dead,Dead],[Dead,Dead,Alive,Dead,Dead],[Dead,Dead,Alive,Dead,Dead],[Dead,Dead,Alive,Dead,Dead],[Dead,Dead,Dead,Dead,Dead]]

block :: [[CellState]]
block = [[Dead,Dead,Dead,Dead],[Dead,Alive,Alive,Dead],[Dead,Alive,Alive,Dead],[Dead,Dead,Dead,Dead]]


-- Events

-- inspired by https://github.com/alexbooth/Game-of-Life-Haskell
handleKeys :: Event -> Game -> Game


-- starts/stops simulation on spacekey down
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {isRunning = not (isRunning game)}

-- step one iteration/stop if running on key 's'
handleKeys (EventKey (Char 's') Down _ _) game = if isRunning game then game {isRunning = False } else game { grid = newGeneration (grid game), isRunning = False, generation = generation game +1}

-- resets game to original state on key 'r'
handleKeys (EventKey (Char 'r') Down _ _) game = game { grid = originState game, isRunning = False, generation = 0}

-- increase simulation steps and frames per second by 5 on keyup (pfeiltaste hoch)
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { fps = changeFps (fps game) 5}

-- decrease simulation steps and frames per second by 5 on keydown (pfeiltaste runter)
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { fps = changeFps (fps game) (-5)}

-- change the cellstate at the cursor position on leftmousebutton down
handleKeys (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) game = game {grid = translateMouse xPos yPos game}

-- in case no key is pressed to continue game simulation
handleKeys _ game = game

-- | use readFile to read your own textfile and create your own game :)
main :: IO ()
main = do
    str <- readFile "startconfigs/pulsGlider"
    let game = createGameFromFile str
    let window = createWindow game
    play window background 20 game drawMyGame handleKeys render
        
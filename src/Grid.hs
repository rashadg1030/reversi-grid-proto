{-# LANGUAGE NamedFieldPuns #-}

module Grid (start) where 

import GridProto.Classic
import GridProto.Core

import Board
import Types
import Reversi 
import Actions 
import Prelude hiding (map, lookup)
import qualified Data.Map as Map
import qualified Data.Char as C

data GridState = GridState{ inputState :: (Int, Int, Bool), gameState :: GameState }

-- Need function for passing to next player when no moves are available

start :: IO ()
start = runClassic classic

classic :: Classic GridState
classic = Classic
  { title = "Reversi"
  , rows = boardSize + metaSize
  , cols = boardSize + metaSize
  , tilePixelSize = 128
  , backgroundColor = Black2
  , setupFn = return $ GridState{ inputState = (0, 0, False), gameState = startingState }
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = gridMap boardSize metaSize 
  , quitFn = quit
  }
    where
        boardSize = 8
        metaSize = 5

update :: Input -> GridState -> IO GridState
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} GridState{ inputState = inputState@(x, y, click), gameState = gameState@GameState{ currentDisc, currentBoard, frames } }
    | mouseButton == Pressed = return $ action
    | otherwise = return GridState{ inputState = (mx, my, lookupKey keys Escape == Released), gameState = gameState }
    where
        action = if length (possibleMoves currentDisc currentBoard) == 0 then
                    GridState{ inputState = (mx, my, True), gameState = changePlayer gameState }
                 else
                    if elem (mx, my) (possibleMoves currentDisc currentBoard) then
                        GridState{ inputState = (mx, my, True), gameState = play (mx, my) gameState }
                    else 
                        GridState{ inputState = (mx, my, True), gameState = gameState }

gridMap :: Int -> Int -> GridState -> Map (Int, Int) Tile
gridMap bSize mSize state = Map.union (boardMap bSize state) (metaDataMap bSize state)

boardMap :: Int -> GridState -> Map (Int, Int) Tile
boardMap bSize GridState{ inputState = (mx, my, click), gameState = GameState{ currentDisc, currentBoard, frames } } = fromList $ do
    y <- [0..(bSize - 1)]
    x <- [0..(bSize - 1)]
    let color = Just $ if (mx, my) == (x,y) then 
                        if elem (mx, my) (possibleMoves currentDisc currentBoard) then 
                            Green1 
                        else
                            Red1
                       else 
                        if (x + y) `mod` 2 == 0 then 
                            Brown1 
                        else 
                            Brown2
                        

    let (symbol, shape) = case Map.lookup (x,y) currentBoard of
                            Nothing -> (Nothing, Nothing)
                            Just d  -> if d == White then
                                        (Just ('W', White0), Just (Circle, White0))
                                       else
                                        (Just ('B', Black0), Just (Circle, Black0))

    return ((x,y), Tile symbol shape color)

metaDataMap :: Int -> GridState -> Map (Int, Int) Tile
metaDataMap bSize GridState{ inputState, gameState } = Map.union turnTile (Map.union leftScoreTile rightScoreTile)
    where
        leftScoreTile  = twoDigitToTile White0 (bSize, 0) (whiteScore gameState)
        rightScoreTile = twoDigitToTile Black0 (bSize + 3, 0) (blackScore gameState)
        turnTile       = fromList $ [((bSize + 2, 0), Tile (Just (turnChar, Rose2)) (Nothing) (Just Cyan2))]
        turnChar       = if currentDisc gameState == Black then '>' else '<'
        turnColor      = if currentDisc gameState == Black then Black0 else White0 

twoDigitToTile :: Color -> (Int, Int) -> Int -> Map (Int, Int) Tile
twoDigitToTile c (x, y) n = fromList $ [((x, y), tile1), ((x + 1, y), tile2)]
    where
        digit1 = fst $ splitInt n
        digit2 = snd $ splitInt n 
        tile1  = Tile (Just (digit1, c)) (Nothing) (Just Cyan2)
        tile2  = Tile (Just (digit2, c)) (Nothing) (Just Cyan2)

splitInt :: Int -> (Char, Char)
splitInt n = if (length $ show n) == 1 then
                ('0', C.intToDigit n)
             else             
                digitListToPair $ show n

digitListToPair :: [Char] -> (Char, Char)
digitListToPair [x, y] = (x, y)
digitListToPair _      = ('0', '0')

-- listToPair :: [a] -> (a, a)
-- listToPair [x, y] = (x, y)
-- listToPair _      = ???

discToColor :: Disc -> Color
discToColor White = White1
discToColor Black = Black1

quit :: GridState -> Bool
quit _ = False



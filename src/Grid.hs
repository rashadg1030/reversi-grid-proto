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
  , rows = boardSize
  , cols = boardSize + 1
  , tilePixelSize = 64
  , backgroundColor = Black2
  , setupFn = return $ GridState{ inputState = (0, 0, False), gameState = startingState }
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = gridMap boardSize
  , sfxFn = const []
  , quitFn = quit
  }
    where
        boardSize = 8

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

gridMap :: Int -> GridState -> Map (Int, Int) Tile
gridMap bSize state = placeTilesAt (boardMap bSize state) (0, bSize) (metaDataMap state)

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

metaDataMap :: GridState -> Map (Int, Int) Tile
metaDataMap GridState{ inputState, gameState } = Map.union turnTile (Map.union leftScoreTile rightScoreTile)
    where
        leftScoreTile  = twoDigitToTile White0 (0, 0) (whiteScore gameState)
        rightScoreTile = twoDigitToTile Black0 (3, 0) (blackScore gameState)
        turnTile       = fromList $ [((2, 0), Tile (Just (turnChar, Rose2)) (Nothing) (Just Cyan2))]
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



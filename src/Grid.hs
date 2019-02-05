{-# LANGUAGE NamedFieldPuns #-}

module Grid (start) where 

import GridProto.Classic
import GridProto.Core

import Board
import Types
import Reversi 
import Actions 
import Prelude hiding (map, lookup)
import Data.Map (map, lookup, fromList)
import qualified Data.Char as C

data GridState = GridState{ inputState :: (Int, Int, Bool), gameState :: GameState }

-- Need function for passing to next player when no moves are available

start :: IO ()
start = runClassic classic

classic :: Classic GridState
classic = Classic
  { title = "Reversi"
  , rows = sides
  , cols = sides
  , tilePixelSize = 128
  , backgroundColor = Black2
  , setupFn = return $ GridState{ inputState = (0, 0, False), gameState = startingState }
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = (tileMap sides)
  , quitFn = quit
  }
  where
    sides = 11

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


tileMap :: Int -> GridState -> Map (Int, Int) Tile
tileMap sides GridState{ inputState = (mx, my, click), gameState = GameState{ currentDisc, currentBoard, frames } } = fromList $ do
    y <- [0..(sides - 1)]
    x <- [0..(sides - 1)]
    let color = Just $ if x < 8 then
                        if (mx, my) == (x,y) then 
                            if elem (mx, my) (possibleMoves currentDisc currentBoard) then 
                                Green1 
                            else
                                Red1
                        else 
                            if (x + y) `mod` 2 == 0 then 
                                Brown1 
                            else 
                                Brown2
                       else 
                            Blue0

    let (symbol, shape) = case lookup (x,y) currentBoard of
                            Nothing -> (Nothing, Nothing)
                            Just d  -> if d == White then
                                        (Just ('W', White0), Just (Circle, White0))
                                       else
                                        (Just ('B', Black0), Just (Circle, Black0))

    return ((x,y), Tile symbol shape color)

metaData :: GridState -> Map (Int, Int) Tile
metaData GridState{ inputState, gameState } = fromList $ [((9, 0), blackScoreTile), ((10, 0), whiteScoreTile)]
    where
        whiteScoreTile = Tile (Just (C.intToDigit $ whiteScore gameState, White0)) (Just (Square, White0)) (Just Blue0)
        blackScoreTile = Tile (Just (C.intToDigit $ blackScore gameState, Black0)) (Just (Square, Black0)) (Just Blue0)
                                

discToColor :: Disc -> Color
discToColor White = White1
discToColor Black = Black1

quit :: GridState -> Bool
quit _ = False



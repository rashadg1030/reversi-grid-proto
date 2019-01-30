{-# LANGUAGE NamedFieldPuns #-}

module Grid (play) where 

import GridProto.Classic
import GridProto.Core

import Board
import Types
import Reversi 
import Actions 
import Prelude hiding (map, lookup)
import Data.Map (map, lookup, fromList)

data GridState = GridState{ inputState :: (Int, Int, Bool), gameState :: GameState }

play :: IO ()
play = runClassic classic

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
  , tileMapFn = tileMap sides
  , quitFn = quit
  }
  where
    sides = 8

update :: Input -> GridState -> IO GridState
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} GridState{ inputState = inputState@(x, y, click), gameState = gameState@GameState{ currentDisc, currentBoard, frames } }
    | mouseButton == Pressed = return $ if elem (mx, my) (possibleMoves currentDisc currentBoard) then
                                            GridState{ inputState = (mx, my, True), gameState = makePlay (mx, my) gameState }
                                        else 
                                            GridState{ inputState = (mx, my, False), gameState = gameState }
    | otherwise = return GridState{ inputState = (mx, my, lookupKey keys Escape == Released), gameState = gameState }


tileMap :: Int -> GridState -> Map (Int, Int) Tile
tileMap sides GridState{ inputState = (mx, my, click), gameState = GameState{ currentDisc, currentBoard, frames } } = fromList $ do
    y <- [0..(sides - 1)]
    x <- [0..(sides - 1)]
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

    let (symbol, shape) = case lookup (x,y) currentBoard of
                            Nothing -> (Nothing, Nothing)
                            Just d  -> if d == White then
                                        (Just ('W', White1), Just (Circle, White1))
                                       else
                                        (Just ('B', Black1), Just (Circle, Black1))

    return ((x,y), Tile symbol shape color)

discToColor :: Disc -> Color
discToColor White = White1
discToColor Black = Black1

quit :: GridState -> Bool
quit _ = False



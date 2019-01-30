{-# LANGUAGE NamedFieldPuns #-}

module Grid where (play) 

import GridProto.Classic
import GridProto.Core

import Board
import Types
import Reversi (play)
import Actions 
import Prelude hiding (map, lookup)
import Data.Map (map, lookup, fromList)

data GridState = GridState{ inputState :: (Int, Int, Bool), gameState :: GameState }

play :: IO ()
play = runClassic classic

classic :: Classic GridState
classic = GridProto
  { title = "Reversi"
  , rows = sides
  , cols = sides
  , tilePixelSize = 128
  , backgroundColor = Black2
  , setupFn = return $ GridState{ inputState = (0, 0, False), gameState = startingState }
  , updateFn = update
  , cleanupFn = const (return ())
  , tilesMapFn = tileMap sides
  , quitFn = quit
  }
  where
    sides = 8

update :: Input -> GridState -> IO GridState
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} _
    | mouseButton == Pressed = return GridState{ inputState = (mx, my, True), gameState = play (mx, my) gameState }
    | otherwise = return (mx, my, lookupKey keys Escape == Released)


tileMap :: Int -> GridState -> Map (Int, Int) Tile
tileMap sides GridState{ inputState, gameState } = fromList $ do
    y <- [0..(sides - 1)]
    x <- [0..(sides - 1)]
    let color = Just $
        if (mx,my) == (x,y)
            -- Mouse color
            then if click then Green1 else Red1
            -- Alternate background colors
            else if (x + y) `mod` 2 == 0 then Brown1 else Brown2
    let (symbol, shape) = 
        case lookup (x,y) (currentBoard gameState) of
            Nothing -> (Nothing, Nothing)
            Just d  -> if d == White then
                            (Just ('w', White1), Just (Circle, White1))
                            else
                            (Just ('!', Black1), Just (Circle, Black1))


    return ((x,y), Tile symbol shape color)


-- if (x + y) `mod` 2 == 1
--     then (Nothing, Nothing)
--     else if y >= 0 && y <= 2
--     then (Just ('w', White1), Just (Circle,  White1))
--     else if y >= 5 && y <= 7
--         then (Just ('!', Black1), Just (Circle, Black1))
--         else (Nothing, Nothing)


-- cells :: Int -> GridState -> Map (Int, Int) G.Cell
-- cells sides GridState { inputState, gameState } = fromList $ do
--   y <- [0..sides]
--   x <- [0..sides]
--   let color = Just $ 
--         if (mouseX inputState, mouseY inputState) == (x,y)
--           then if mouseClick inputState then Green1 else Red1
--           else if (x+y) `mod` 2 == 0 then Brown1 else Brown2
  
--   let shape = 
        -- case lookup (x,y) (currentBoard gameState) of
        --   Nothing -> Nothing
        --   Just d  -> if d == T.White then
        --                   Just (FillCircle, G.White)
        --                 else
        --                   Just (FillCircle, G.Black)

--   return ((x,y), Cell shape color)

discToColor :: Disc -> Color
discToColor T.White = G.White
discToColor T.Black = G.Black

quit :: GridState -> Bool
quit _ = False
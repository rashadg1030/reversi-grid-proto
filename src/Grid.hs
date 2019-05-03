{-# LANGUAGE NamedFieldPuns #-}

module Grid (start) where 

import GridProto.Classic
import GridProto.Core

import Reversi.Board
import Reversi.Types
--import Reversi 
import Reversi.Actions 
import Reversi.GameTree
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
  , rows = boardSize + 1
  , cols = boardSize
  , tilePixelSize = 64
  , backgroundColor = Cyan2
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
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} GridState{ inputState = inputState@(x, y, click), gameState = gameState@GameState{ getDisc, getBoard, getMove, getFrames } }
    | getDisc == White = return GridState{ inputState = (mx, my, True), gameState = play (moveToLoc . runMinmax . refresh $ gameState) gameState }
    | mouseButton == Pressed = return action
    | otherwise = return GridState{ inputState = (mx, my, lookupKey keys Escape == Released), gameState = gameState }
    where
        action = if length (possibleMoves getDisc getBoard) == 0 then
                    GridState{ inputState = (mx, my, True), gameState = pass gameState }
                 else
                    if elem (mx, my) (possibleMoves getDisc getBoard) then
                        GridState{ inputState = (mx, my, True), gameState = play (mx, my) gameState }
                    else 
                        GridState{ inputState = (mx, my, True), gameState = gameState }

gridMap :: Int -> GridState -> Map (Int, Int) Tile
gridMap bSize state = placeTilesAt (boardMap bSize state) (1, bSize) (metaDataMap state)

boardMap :: Int -> GridState -> Map (Int, Int) Tile
boardMap bSize GridState{ inputState = (mx, my, click), gameState = GameState{ getDisc, getBoard, getMove, getFrames } } = fromList $ do
    y <- [0..(bSize - 1)]
    x <- [0..(bSize - 1)]
    let color = Just $ if (mx, my) == (x,y) then 
                        if elem (mx, my) (possibleMoves getDisc getBoard) then 
                            Green1 
                        else
                            Red1
                       else 
                        if (x + y) `mod` 2 == 0 then 
                            Brown1 
                        else 
                            Brown2
                        

    let (symbol, shape) = case Map.lookup (x,y) getBoard of
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
        rightScoreTile = twoDigitToTile Black0 (4, 0) (blackScore gameState)
        turnTile       = fromList $ zipWith (\x ch -> ((x, 0), Tile (Just (ch, Rose2)) Nothing Nothing)) [2..] turnString
        turnString     = if getDisc gameState == Black then "->" else "<-"

whiteScore :: GameState -> Int
whiteScore = countDiscs White . getBoard

blackScore :: GameState -> Int
blackScore = countDiscs Black . getBoard

twoDigitToTile :: Color -> (Int, Int) -> Int -> Map (Int, Int) Tile
twoDigitToTile c (x, y) n = fromList $ [((x, y), tile1), ((x + 1, y), tile2)]
    where
        digit1 = fst $ splitInt n
        digit2 = snd $ splitInt n 
        tile1  = Tile (Just (digit1, c)) Nothing Nothing
        tile2  = Tile (Just (digit2, c)) Nothing Nothing

splitInt :: Int -> (Char, Char)
splitInt n = if (length $ show n) == 1 then
                ('0', C.intToDigit n)
             else             
                digitListToPair $ show n

digitListToPair :: [Char] -> (Char, Char)
digitListToPair [x, y] = (x, y)
digitListToPair _      = ('0', '0')

discToColor :: Disc -> Color
discToColor White = White1
discToColor Black = Black1

quit :: GridState -> Bool
quit _ = False



{-# LANGUAGE TemplateHaskell #-}
module HitAndBlow
  ( Game(..)
  , Numbers
  , Hint
  , initGame
  , chkAnswer
  , mkNumbers
  , allNumbers
  , numbers, hint, done
  , userGuess, correctAnswer, hints
  ) where

import Data.List (delete)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Lens.Micro ((^.), (.~), (%~), (&))
import Lens.Micro.TH

-- Types

newtype Numbers = Numbers [Char]
  deriving (Eq, Show)

newtype Hint = Hint (Int, Int) -- (Hit, Blow)
  deriving (Eq, Show)

data Game = Game
  { _userGuess     :: Numbers
  , _correctAnswer :: Numbers
  , _hints         :: [(Numbers, Hint)]
  , _done          :: Bool
  } deriving (Show)

makeLenses ''Game

initGame :: IO Game
initGame = do
  ca <- mkCorrectAnswer
  let g = Game
            { _userGuess     = Numbers "0000"
            , _correctAnswer = ca
            , _hints         = []
            , _done          = False
            }
  return g

chkAnswer :: Game -> String -> Maybe Game
chkAnswer g s = do
  n <- mkNumbers s
  let h@(Hint (hit, _))  = getHint n (g ^. correctAnswer)
      g' = g & userGuess .~ n
             & hints     %~ ((n, h):)
             & done      .~ (hit == 4)
  return g'

mkCorrectAnswer :: IO Numbers
mkCorrectAnswer = do
  t <- (round . (*1000)) <$> getPOSIXTime
  let n = length allNumbers
  return $ Numbers . (allNumbers !!) $ t `mod` n

mkNumbers :: String -> Maybe Numbers
mkNumbers s =
  if isNumbers s
    then Just (Numbers s)
    else Nothing
  where
    isNumbers = (`elem` allNumbers)

numbers :: Numbers -> [Char]
numbers (Numbers n) = n

hint :: Hint -> (Int, Int)
hint (Hint (h, b)) = (h, b)

allNumbers :: [[Char]]
allNumbers = permut 4 ['0'..'9']

getHint :: Numbers -> Numbers -> Hint
getHint (Numbers xs) (Numbers ys) = Hint (hit, blow)
  where
    hit  = length [ x | (x, y) <- zip xs ys, x == y ]
    tmp  = length [ x | x <- xs, y <- ys, x == y ]
    blow = tmp - hit

-- Utility functions

permut :: Eq a => Int -> [a] -> [[a]]
permut 0 _  = [[]]
permut n xs = do
  y <- xs
  let ys = delete y xs
  (y:) <$> permut (n-1) ys
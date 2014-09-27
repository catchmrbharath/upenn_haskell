{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int
  deriving (Show, Ord, Num, Eq)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score::Char -> Score
score a
  | x `elem` "aeiounrslt" = Score 1
  | x `elem` "gd" = Score 2
  | x `elem` "mbcp" = Score 3
  | x `elem` "yfvwh" = Score 4
  | x == 'k' = Score 5
  | x `elem` "jx" = Score 8
  | x `elem` "qz" = Score 10
  | otherwise = Score 0
    where x = toLower a


scoreString:: String -> Score
scoreString str = sum (map score str)

getScore :: Score -> Int
getScore (Score i) = i




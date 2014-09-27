{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinListBuffer where
  
import Data.Monoid
import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString x = foldr ((+++) . scoreLine) Empty (lines x)
  line = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines = getSize . size . tag
  value  = getScore . fst . tag



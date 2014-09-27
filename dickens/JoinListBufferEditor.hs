module Main where

import JoinListBuffer
import Editor
import JoinList
import Sized
import Scrabble

main = runEditor editor $ Single (Score 1, Size 1) "a"

module JoinList where 

import Data.Monoid
import Sized
import Scrabble
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
      deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mappend (tag x) (tag y)) x y

indexJHelper::(Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJHelper index (Append _ x y)
  | index <= getSize (size (tag x)) = indexJHelper index x
  | otherwise = indexJHelper (index - getSize (size (tag x))) y
indexJHelper _ Empty = Nothing
indexJHelper index (Single _ a)
  | index == 1 = Just a
  | otherwise = Nothing

indexJ :: (Sized b, Monoid b) => Int ->  JoinList b a -> Maybe a
indexJ i = indexJHelper (i + 1)

jlToList :: JoinList m a -> [a]
jlToList Empty =  []
jlToList (Single _ a) = [a]
jlToList (Append _ x y) = jlToList x ++ jlToList y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 x = x
dropJ 1 (Single _ _) = Empty
dropJ n (Append _ x y)
  | n == left = y
  | n > left = dropJ left x +++ dropJ (n - left) y
  | otherwise = dropJ n x +++ y
    where
      left = getSize (size (tag x))
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ (Single x y) = Single x y
takeJ n (Append _ x y)
  | n > left = x +++ takeJ (n - left) y
  | otherwise = takeJ n x
    where
      left = getSize (size (tag x))
takeJ _ _ = Empty


{-- scrabble stuff --}

scoreLine::String -> JoinList (Score, Size) String
scoreLine x = Single (scoreString x, Size 1) x







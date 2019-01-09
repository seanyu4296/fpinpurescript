module Chapter2 (
  formatAbs,
  factorial,
  formatResult,
  findFirst',
  isSorted,
  compose
) where 

import Prelude (negate, otherwise, show, (*), (+), (-), (<=), (<>), (==), (>), (>=))
import Data.Array (length, index)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

abs :: Int -> Int
abs x = if ( x > 0) then -x else x



factorial :: Int -> Int
factorial n = go n 1
  where
    go :: Int -> Int -> Int
    go x acc | x <= 0    = acc
             | otherwise = go (x - 1) (x * acc)


formatResult :: String -> Int -> (Int -> Int) -> String
formatResult name n f = "The " <> name <> " of " <> show n <> " is " <> show (f n)

findFirst :: Array String -> String -> Int
findFirst ss key = loop 0
  where
    loop :: Int -> Int
    loop n | n >= length ss = -1
          | index ss n == Just (key) = n
          | otherwise = loop n + 1

findFirst' :: forall a. Array a -> (a -> Boolean) -> Int
findFirst' as p = loop 0
  where
    compare :: forall b. Maybe b -> (b -> Boolean) -> Boolean
    compare a f = case a of
      Just x -> f x
      _ -> false
    loop :: Int -> Int
    loop n | n >= length as = -1
          | compare (index as n) p = n
          | otherwise = loop n + 1

isSorted :: forall a. Array a -> (a -> a -> Boolean) -> Boolean
isSorted [] _ = true
isSorted as f = loop 0
  where
    compare :: (a -> a -> Boolean) -> Maybe a -> Maybe a -> Boolean
    compare g first last = case Tuple first last of
      Tuple (Just x) (Just y) -> g x y
      Tuple (Just x) (Nothing) -> true
      _ -> true
    loop :: Int -> Boolean
    loop n | compare f (index as n) (index as (n+1)) = loop (n + 1)
          | otherwise = false

curry :: forall a b c. (Tuple a b -> c)-> (a -> b -> c) 
curry f x y = f (Tuple x y)

uncurry:: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f x = f (fst x) (snd x)

compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g a = f (g a)

formatAbs :: Int -> String
formatAbs x = "The absolute value of " <> show x <> " is " <> show (abs x)

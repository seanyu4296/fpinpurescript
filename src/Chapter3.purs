module Chapter3 where

import Data.Generic.Rep
import Prelude

import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))


data List' a = Nil' | Cons' a (List' a)


sum :: List' Int -> Int
sum Nil' = 0
sum (Cons' x xs) = x + (sum xs)

product :: List' Number -> Number
product Nil' = 1.0
product (Cons' x xs) = x * product xs

tail :: forall a. List' a -> List' a
tail Nil' = Nil'
tail (Cons' x xs) = xs

setHead :: forall a. a -> List' a -> List' a
setHead a Nil' = Cons' a Nil'
setHead a (Cons' x xs) = Cons' a xs

drop :: forall a. List' a -> Int -> List' a
drop Nil' _ = Nil'
drop (Cons' x xs) 0 = xs
drop (Cons' x xs) i = drop xs (i - 1)

dropWhile :: forall a. List' a -> (a -> Boolean) -> List' a
dropWhile l f = case l of
  Cons' x xs | f x -> dropWhile xs f
  _ -> l
                          
init :: forall a. List' a -> List' a
init Nil' = Nil'
init (Cons' x Nil') = Nil'
init (Cons' x xs) = Cons' x (init xs)

foldRight :: forall a ac. List' a -> ac -> (a -> ac -> ac) -> ac
foldRight as acc f = case as of
  Nil' -> acc
  Cons' x xs -> f x (foldRight xs acc f)
-- 10
-- (\3 y -> y - 3) y = (foldRight (Nil)) 10 (\x y -> y -x)
-- (\2 y -> y -2) y = (foldRight (Cons' 3: Nil)) 10 (\x y -> y -x)
-- (\1 y -> y -1)  y = (foldRight (Cons' 2 (Cons' 3: nil)) 10 (\x y -> y -x))


foldLeft :: forall a ac. List' a -> ac -> (ac -> a -> ac) -> ac
foldLeft as acc f = case as of
  Nil' -> acc
  Cons' x xs -> foldLeft xs (f acc x) f

foldLeft' :: forall a ac. List' a -> ac -> (ac -> a -> ac) -> ac
foldLeft' l acc f = foldRight (reverse l) acc (\c ac -> f ac c)
-- you can do foldLeft with foldRight no reverse

reverse :: forall a. List' a -> List' a
reverse l = foldLeft l Nil' (\acc curr -> curr : acc)

-- correct
foldRight' :: forall a ac. List' a -> ac -> (a -> ac -> ac) -> ac
foldRight' l acc f = foldLeft (reverse l) acc (\ac c -> f c ac)
-- another way to implement foldRight using foldLeft
--

sum'':: List' Int -> Int
sum'' l = foldLeft l 0 (\acc y -> acc + y)

product'':: List' Int -> Int
product'' l = foldRight l 1 (\acc x -> acc * x)

append :: forall a. List' a -> List' a -> List' a
append la la' = foldLeft (reverse la) la' (\acc y -> y: acc)
-- 1 : 2: 3 : Nil'  4: 5: 6: Nil'

append':: forall a. List' a -> List' a -> List' a
append' la la' = foldRight la la' (\y acc -> y: acc)


concat :: forall a. List' (List' a) -> List' a
concat la = foldRight la Nil' append

{- foldRight' :: forall a ac. List' a -> ac -> (a -> ac -> ac) -> ac
foldright' = foldLeft -}
sum' :: List' Int -> Int
sum' l = foldRight l 0 (\x y -> x + y)

product':: List' Int -> Int
product' l = foldRight l 1 (\x y -> x * y)

length' :: forall a. List' a -> Int
length' l = foldRight l 0 (\x y -> y + 1)

map :: forall a b. (a -> b) -> List' a -> List' b
map f l = case l of
  Nil' -> Nil'
  Cons' x xs -> Cons' (f x) (map f xs)

map' :: forall a b. (a -> b) -> List' a -> List' b
map' f l = foldLeft (reverse l) Nil' (\acc y -> f y : acc)



filter :: forall a. (a -> Boolean) -> List' a -> List' a
filter f l = case l of
  Nil' -> Nil'
  Cons' x xs | f x -> Cons' x (filter f xs)
              | otherwise -> (filter f xs)

flatMap :: forall a b. (a -> List' b) -> List' a -> List' b
flatMap f l = concat (map f l)

filter' :: forall a. (a -> Boolean) -> List' a -> List' a
filter' f l = flatMap (\x -> if (f x) then x : Nil' else Nil') l

zipWith :: forall a b c. (a -> b -> c) -> List' a -> List' b -> List' c
zipWith f a b = case Tuple a b of 
  Tuple (Cons' x xs) (Cons' y ys) -> Cons' (f x y) (zipWith f xs ys)
  Tuple _ _ -> Nil'
-- 1 : 2: 3 : Nil , 4: 5: 6 : Nil, \x y -> x + y = 5 7 9 

hasSubsequence :: List' Int -> List' Int -> Boolean
hasSubsequence la la' = loop la la' la'
  where
    loop :: List' Int -> List' Int -> List' Int -> Boolean
    loop a b c = case a, b of
      (Cons' x xs),(Cons' y ys) | x == y -> loop xs ys c
                                | otherwise -> loop (xs) c c
      (Cons' x xs), Nil' -> true
      Nil', Nil' -> true
      Nil', (Cons' _ _) -> false

{- startsWith:: List' Int -> List' Int -> Boolean
startsWith a b = case a, b of
  _ , Nil' -> true
  (Cons' d e), (Cons' f g) | d == f -> startsWith e g
  _, _ -> false

hasSubsequence' :: List' Int -> List' Int -> Boolean
hasSubsequence' a b = case a of 
  Nil' -> b == Nil'
  _ | startsWith a b -> true
  Cons' x y -> hasSubsequence' y b -}

-- first implementation of hasSubsequence
{- hasSubsequence a b = case a, b of
  (Cons' x xs),(Cons' y ys) | x == y -> hasSubsequence xs ys 
                            | otherwise -> hasSubsequence (xs) (Cons' y ys)
  (Cons' x xs), Nil' -> true
  Nil', Nil' -> true
  Nil', (Cons' _ _) -> false -}



instance showList' :: Show a => Show (List' a) where
  show Nil' = "Nil'"
  show (Cons' x xs) = "Cons(" <> show x <> "," <> show xs <> ")"



infixr 8 Cons' as :


data Tree a = Leaf a | Branch (Tree a) (Tree a)
{- instance showTree :: Show a => Show (Tree a ) where 
  show Leaf a = "Leaf(" <> show a <> ")"   
  show (Branch x y) = "Branch(" <> show x <> "," show y <> ")" -}

derive instance genericTree :: Generic (Tree a) _
instance showTree :: Show a => Show (Tree a ) where 
  show x = genericShow x

-- size:: forall a. Tree a -> Int
-- size t = loop t 0
--   where 
--     loop :: Tree a -> Int -> Int
--     loop (Leaf a) _ = 1
--     loop (Branch (Leaf _ ) (Leaf _ )) acc = acc + 2
--     loop (Branch (Branch _ _) (Branch _ _)) acc = 0
--     loop (Branch (Leaf b ) y) acc = 1 + loop y acc 
--     loop (Branch x (Leaf c)) acc = 1 + loop x acc

size':: forall a. Tree a -> Int
size' t = case t of
  Leaf _ -> 1
  Branch x y -> 1 + size' x + size' y


maximum :: Tree Int -> Int
maximum t = case t of
  Leaf x -> x
  -- Branch (Leaf x) y -> if x > maximum y then x else maximum y
  -- Branch x (Leaf y) -> if y > maximum x then y else maximum x
  Branch x y -> if maximum x > maximum y then maximum x else maximum y

depth :: forall a. Tree a -> Int
depth t = case t of
  Leaf _ -> 0
--   Branch (Branch x y) (Branch w z) -> if (1 + depth x) > (1 + depth y) then (1 + depth x)  else (1 + depth y)
  Branch x y -> 1 + max (depth x) (depth y)

mapTree :: forall a b. (a -> b) -> Tree a -> Tree b
mapTree f t = case t of
  Leaf x -> Leaf (f x)
  Branch y z -> Branch (mapTree f y) (mapTree f z)

fold :: forall a b. Tree a -> (a -> b) -> (b ->b -> b) -> b
fold t f g= case t of
  Leaf x -> f x
  Branch y z -> g (fold y f g) (fold z f g)

mapTree' :: forall a b. (a -> b) -> Tree a -> Tree b
-- fold :: forall a b. Tree a -> (a -> Tree b) -> (Tree b -> Tree b -> Tree b) -> Tree b
mapTree' f t = fold t (\x -> Leaf (f x)) Branch

maximum':: Tree Int -> Int
-- fold :: forall a b. Tree a -> (a -> Int) -> (Int -> Int -> Int) -> Int
maximum' t = fold t (\x -> x) (\x y -> if x > y then x else y) 

depth':: forall a. Tree a -> Int
-- fold :: forall a b. Tree a -> (a -> Int) -> (Int -> Int -> Int) -> Int
depth' t = fold t (\x -> 0) (\x y -> 1 + (max x y))


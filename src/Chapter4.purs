module Chapter4 where 

import Data.Generic.Rep
import Prelude

import Chapter3 (List'(..), foldLeft, map', (:))
import Data.Generic.Rep.Show (genericShow)
import Math (pow)

data Option a = Some a | None
derive instance genericOption :: Generic (Option a) _
instance showOption :: Show a => Show (Option a ) where 
  show x = genericShow x

map :: forall a b. (a -> b) -> Option a -> Option b
map f a = case a of
  Some x -> Some (f x)
  None -> None


filter :: forall a. (a -> Boolean) -> Option a -> Option a
filter f None = None
filter f (Some x) | f x = Some x
                  | otherwise = None

getOrElse :: forall a. a -> Option a -> a
getOrElse default x = case x of
  None -> default
  Some y -> y

flatMap :: forall a b. (a -> Option b) -> Option a -> Option b
flatMap f x = case x of
  Some y -> f y
  None -> None

orElse :: forall a. Option a -> Option a -> Option a
orElse a b = case a of
  None -> b
  _ -> a


variance :: List' Number -> Option Number
variance l = flatMap (\m -> mean (map' (\x -> let n = x - m
                                                  n2 = n * n 
                                              in n2) l )) (mean l)
  where
    mean :: List' Number -> Option Number
    mean la = case la of
      Nil' -> None
      x -> Some ((foldLeft x 0.0 (\acc y -> acc + y)) / 2.0)


map2 :: forall a b c. Option a -> Option b -> (a -> b -> c) -> Option c
map2 x y f = case x, y of
  Some a, Some b -> Some (f a b)
  _ , _ -> None  

sequence :: forall a. List' (Option a) -> Option (List' a)
sequence l = foldLeft l None (\acc x -> case acc, x of
  Some u, Some t -> Some (t : u)
  None, Some t -> Some (t : Nil')
  _, _ -> None
)


traverse :: forall a b. List' a -> (a -> Option b) -> Option (List' b)
traverse l f = sequence (map' f l)

traverse' :: forall a b. List' a -> (a -> Option b) -> Option (List' b)
traverse' l f = foldLeft l None (\acc x -> case acc, f x of
  Some u, Some t -> Some (t : u) 
  None, Some t -> Some (t : Nil')
  _, _ -> None
)

sequence' :: forall a. List' (Option a) -> Option (List' a)
sequence' l = traverse' l (\x -> x)
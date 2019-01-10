module Chapter4 where 

import Data.Generic.Rep
import Prelude

import Chapter3 (List'(..), foldLeft, foldRight, map', (:))
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


-- the sense of map2 is accepting two things if both are valid apply the function
map2 :: forall a b c. Option a -> Option b -> (a -> b -> c) -> Option c
map2 x y f = case x, y of
  Some a, Some b -> Some (f a b)
  _ , _ -> None  


-- a good example is List (Future a) -> Future (List a) 
-- if empty list pass over empty list
--
sequence :: forall a. List' (Option a) -> Option (List' a)
sequence l = foldRight l (Some Nil') (\x acc -> map2 x acc (:)) 

{- (\acc x -> case acc, x of
  Some u, Some t -> Some (t : u)
  _, _ -> None
) -}


traverse :: forall a b. List' a -> (a -> Option b) -> Option (List' b)
traverse l f = sequence (map' f l)

traverse' :: forall a b. List' a -> (a -> Option b) -> Option (List' b)
traverse' l f = foldRight l (Some Nil') (\x acc -> map2 (f x) acc (:))


{- (\acc x -> case acc, f x of
  Some u, Some t -> Some (t : u) 
  None, Some t -> Some (t : Nil')
  _, _ -> None
) -}

sequence' :: forall a. List' (Option a) -> Option (List' a)
sequence' l = traverse' l identity

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show x = genericShow x

mapE :: forall l r a. (r -> a) -> Either l r -> Either l a
mapE f (Right x) = Right (f x)
mapE f (Left y) = Left y

flatMapE :: forall l r a. (r -> Either l a) -> Either l r -> Either l a
flatMapE f x = case x of
  Right y -> f y
  Left z -> Left z

orElseE :: forall l r. Either l r -> Either l r -> Either l r
orElseE (Right x) _ = Right x
orElseE _ y = y

map2E :: forall l r r2 r3. Either l r -> Either l r2 -> (r -> r2 -> r3) -> Either l r3
map2E (Right r) (Right r2) f = Right (f r r2)
map2E (Left l) _ _ = Left l
map2E _ (Left l2) _ = Left l2

sequenceE :: forall l r. List' (Either l r) -> Either l (List' r)
sequenceE l = foldRight l (Right Nil') (\x acc -> map2E x acc (:))

traverseE :: forall a l r. List' a -> (a -> Either l r) -> Either l (List' r)
traverseE l f = foldRight l (Right Nil') (\x acc -> map2E (f x) acc (:))

-- REPRESENTING EFFECTS AS VALUES
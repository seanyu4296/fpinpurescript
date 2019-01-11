module Chapter5 where 

import Prelude

import Chapter3 (List'(..), (:))
import Chapter4 (Option(..))
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, unwrap, wrap)




newtype Stream a = Stream (Lazy (Step a))

derive instance streamNewType :: Newtype (Stream a) _

data Step a = SEmpty | SCons a (Stream a)


{- stringStream :: Stream String
stringStream = defer $ \_ -> SCons "a" $ defer (\_ -> SEmpty) 
 -}
{- sample :: Step Int
sample = SCons 1 (defer (\_ -> SEmpty) ) -}

headOption :: forall a. Stream a -> Option a
headOption s =  case force $ unwrap s of 
  SEmpty -> None
  SCons h t -> Some h

fromList :: forall a. List' a -> Stream a
fromList l = wrap $ case l of
  Nil' -> defer(\_ -> SEmpty)
  Cons' x xs-> defer (\_ -> SCons x $ fromList xs)


toList :: forall a. Stream a -> List' a
toList s = case force (unwrap s) of
  SCons x xs -> x : toList xs
  SEmpty -> Nil'


take :: forall a. Int -> Stream a -> Stream a
take n s = Stream $ defer (\_ -> case n, force (unwrap s) of
  0, _ ->  SEmpty
  _, SCons x xs -> SCons x (take (n - 1) xs)
  _, SEmpty -> SEmpty
)

takeWhile :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile f s = Stream $ defer $ \_ -> case force $ unwrap s of
  SEmpty -> SEmpty 
  SCons h t | f h -> SCons h (takeWhile f t)
            | otherwise -> SEmpty

foldRightS :: forall a b. (a -> b -> b) -> Lazy b -> Stream a -> b
foldRightS f accL s = case force $ unwrap s of 
  SEmpty -> force accL
  SCons h t  -> f h (foldRightS f accL t)

exists :: forall a. (a -> Boolean) -> Stream a -> Boolean
exists f s = foldRightS (\x acc -> f x || acc) (defer (\_ -> false)) s

forAll :: forall a. (a -> Boolean) -> Stream a -> Boolean
forAll f s = foldRightS (\x acc -> f x && acc) (defer(\_ -> false)) s

takeWhile' :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile' f s = foldRightS (\x acc -> if f x then Stream $ defer(\_ -> SCons x acc) else Stream $ defer (\_ -> SEmpty)) (defer(\_ -> Stream $ defer (\_ -> SEmpty))) s
-- Stream a
-- a = a
-- b = Stream x 
-- Lazy b = Lazy Stream x

{- takeWhile :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile f s = case s of
  Empty -> Empty
  Conss h t | f (h unit) -> Conss h (\x -> takeWhile f (t unit))
            | otherwise -> Empty
 -}



{- infixr 8 SCons as ~
infixr 8 defer as ~>
 -}




{- deferrer :: forall a. a -> Lazy a
deferrer a = defer (\_ -> a)
 -}
{- cons :: forall a. (Unit -> a) -> (Unit -> Stream a) -> Stream a 
cons = Conss  -}
{- c :: forall a. (Unit -> a) -> Stream a -> Stream a
c x y = Conss x (\_ -> y)
data Stream a = Empty | Conss (Unit -> a) (Unit -> Stream a)
headOption :: forall a. Stream a -> Option a
headOption Empty = None
headOption (Conss h t) = Some (h unit)

toList :: forall a. Stream a -> List' a
toList Empty = Nil'
toList (Conss h t) = Cons'(h unit) (toList (t unit))

take :: forall a. Int -> Stream a -> Stream a
take n s = case n, s of
  0, _ -> Empty
  _, Conss h t ->  Conss h (\x -> take (n - 1) (t unit))
  _, Empty -> Empty




foldRightS :: forall a b. (a -> b -> b) -> (Unit -> b) -> Stream a -> b
foldRightS f acc s = case s of
  Conss h t -> f (h unit) (foldRightS f acc (t unit))
  _ -> acc unit

exists :: forall a. (a -> Boolean) -> Stream a -> Boolean
exists f = foldRightS (\y acc -> f y || acc) (\_ -> false)


forAll :: forall a. (a -> Boolean) -> Stream a -> Boolean
forAll f = foldRightS (\y acc -> f y && acc) (\_ -> true) -}


{- takeWhile' :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile' f s = foldRightS (\y acc -> ) (\_ -> Empty)
 -}

instance bindList :: Bind List where
  bind xs f = Stream (map go (unwrap xs))
    where
      go Empty = Empty
      go (Cons x xs') = force <<< unwrap (append (f x) ( bind xs' f))
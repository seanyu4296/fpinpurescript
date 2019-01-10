module Chapter5 where 

import Prelude

import Chapter3 (List'(..))
import Chapter4 (Option(..))
import Data.Lazy (Lazy, defer)


data Stream a = Empty | Conss (Unit -> a) (Unit -> Stream a)

type Stream' a = Lazy (Step a)
data Step a = Empty' | Cons'' a (Stream' a)



stringS :: Stream' String
stringS = defer $ \_ -> Cons'' "a" $ defer (\_ -> Empty') 


deferrer :: forall a. a -> Lazy a
deferrer a = defer (\_ -> a)

sample :: Step Int
sample = Cons'' 1 (defer (\_ -> Empty') )

{- cons :: forall a. (Unit -> a) -> (Unit -> Stream a) -> Stream a 
cons = Conss  -}
c :: forall a. (Unit -> a) -> Stream a -> Stream a
c x y = Conss x (\_ -> y)

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


takeWhile :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile f s = case s of
  Empty -> Empty
  Conss h t | f (h unit) -> Conss h (\x -> takeWhile f (t unit))
            | otherwise -> Empty

foldRightS :: forall a b. (a -> b -> b) -> (Unit -> b) -> Stream a -> b
foldRightS f acc s = case s of
  Conss h t -> f (h unit) (foldRightS f acc (t unit))
  _ -> acc unit

exists :: forall a. (a -> Boolean) -> Stream a -> Boolean
exists f = foldRightS (\y acc -> f y || acc) (\_ -> false)


forAll :: forall a. (a -> Boolean) -> Stream a -> Boolean
forAll f = foldRightS (\y acc -> f y && acc) (\_ -> true)


{- takeWhile' :: forall a. (a -> Boolean) -> Stream a -> Stream a
takeWhile' f s = foldRightS (\y acc -> ) (\_ -> Empty)
 -}
infixr 8 c as |>
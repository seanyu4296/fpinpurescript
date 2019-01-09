module Main where

import Prelude

import Chapter3 (List'(..), (:))
import Chapter4 (Option(..), filter, flatMap, getOrElse, map, orElse, variance, sequence)
import Effect (Effect)
import Effect.Console (log)

myPrint :: forall a. Show a => a -> Effect Unit
myPrint = log <<< show

{- sl :: List' Int
sl = 1:3:3:2: Nil'
st :: Tree Int
st = Branch (Branch(Branch(Leaf 1) (Leaf 2)) (Branch(Leaf 1) (Leaf 2))) (Leaf 4)
 -}
o :: Option Int
o = Some 5

n :: Option Int
n = None

main :: Effect Unit
main = do
  myPrint (sequence (Some(1): Some(2): Nil'))
  myPrint (variance (1.0:2.0: Nil'))
  myPrint (map (_ + 1) o)
  myPrint (filter (_ > 1) o)
  myPrint (getOrElse 1 n)
  myPrint (flatMap (\x -> o) o)
  myPrint (orElse o n)
  -- myPrint (mapTree (\x -> x +1) st)
  -- myPrint (maximum st)
  -- myPrint (depth st)
  -- myPrint (map)
  -- myPrint (size' st)
  -- myPrint (hasSubsequence sl (1:2:Nil'))
  -- myPrint (zipWith (\a b -> a + b) sl sl)
  -- myPrint (filter' (\x -> x > 1) sl )
  -- myPrint (concat ((4: Nil'): (3: Nil'): Nil'))
  -- myPrint (filter (\x -> x > 1) sl )
  -- myPrint (flatMap (\x -> 1 : Nil') sl )
  -- myPrint (dropWhile (sl)(\x -> x < 2))
  -- myPrint (foldRight (sl) 10 (\x y -> y -x))
  -- myPrint (append (sl) (4:5:6: Nil'))
  -- myPrint (append' (sl) (4:5:6: Nil'))
  -- myPrint (reverse (sl))
  -- myPrint (length' (sl))
  -- myPrint (foldRight' (sl) Nil' (\curr acc -> curr : acc))
  -- myPrint "1"
  -- myPrint (foldRight (sl) 10 \b acc -> acc - b) 
  -- myPrint (init (1:2:3: 4: Nil'))
  -- myPrint ((drop (1:2: 3: Nil')) 1)
  -- myPrint $ isSorted [1,2] \x y -> x <= y
  -- myPrint $ findFirst' ["1"] \x -> x == "1" 
  -- myPrint $ formatResult "factorial" 5 factorial

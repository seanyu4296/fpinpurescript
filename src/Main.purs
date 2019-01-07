module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Chapter2 ( factorial, formatResult, findFirst', isSorted)

main :: Effect Unit
main = do
  log (show (isSorted [1,2] \x y -> x <= y))
  log ( show (findFirst' ["1"] \x -> x == "1"))
  log ( formatResult "factorial" 5 factorial ) 

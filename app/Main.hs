module Main where

import qualified ApproximationRules
import Data.Maybe


main :: IO ()
main = do
  print(fromJust ( ApproximationRules.trapezoidal 0.0 3.0 5 (\x -> x*x + 1.0) ) ) 

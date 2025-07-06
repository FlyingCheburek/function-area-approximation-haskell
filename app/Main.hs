module Main where

import qualified ApproximationRules
import Data.Maybe


main :: IO ()
main = do
  print(fromJust ( ApproximationRules.simpson 1.0 5.0 4 (\x -> 1.0/x) ) ) 

module Main where

import qualified ApproximationRules
import Data.Maybe


main :: IO ()
main = do
  print(fromJust ( ApproximationRules.averagesum (pi/2.0) (3.0*pi/2.0) 4 (\x -> -1 * cos x) ) ) 

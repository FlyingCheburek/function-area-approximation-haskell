module ApproximationRules (
    leftsum',
    leftsum,
    rightsum',
    rightsum,
    averagesum',
    averagesum
) where

leftsum' :: Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> Double -- auxiliary function, for recursion
leftsum' a b area step x f | x < b = leftsum' a b (area+step*(f x)) step (x+step) f
                          | otherwise = area

leftsum :: Double -> Double -> Int -> (Double -> Double) -> Maybe Double 
leftsum a b n f | n <= 0 = Nothing -- n should be non-zero positive.
                | a >= b = Nothing -- interval [a,b] should have limits such as a < b.
                | otherwise = Just( leftsum' a b 0.0 ((b-a)/(fromIntegral n)) a f )  

rightsum' :: Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> Double -- auxiliary function, for recursion
rightsum' a b area step x f | x < b = rightsum' a b (area+step*(f (x+step))) step (x+step) f 
                            | otherwise = area

rightsum :: Double -> Double -> Int -> (Double -> Double) -> Maybe Double
rightsum a b n f | n <= 0 = Nothing -- n should be non-zero positive.
                 | a >= b = Nothing -- interval [a,b] should have limits such as a < b.
                 | otherwise = Just( rightsum' a b 0.0 ((b-a)/(fromIntegral n)) a f )

averagesum' :: Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> Double -- auxiliary function, for recursion
averagesum' a b area step x f | x < b = averagesum' a b (area+step*(f ((x+x+step)/2.0))) step (x+step) f 
                              | otherwise = area 

averagesum :: Double -> Double -> Int -> (Double -> Double) -> Maybe Double
averagesum a b n f | n <= 0 = Nothing -- n should be non-zero positive.
                   | a >= b = Nothing -- interval [a,b] should have limits such as a < b.
                   | otherwise = Just( averagesum' a b 0.0 ((b-a)/(fromIntegral n)) a f ) 
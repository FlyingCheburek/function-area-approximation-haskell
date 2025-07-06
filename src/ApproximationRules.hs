module ApproximationRules (
    leftsum',
    leftsum,
    rightsum',
    rightsum,
    averagesum',
    averagesum,
    trapezoidal',
    trapezoidal,
    simpson',
    simpson,
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

trapezoidal' :: Double -> Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> Double -- auxiliary function, for recursion
trapezoidal' a b n area step x f | x < b = trapezoidal' a b n (area+2.0*(f (x))) step (x+step) f
                                 | otherwise = area * ((b-a)/(2.0*n))

trapezoidal :: Double -> Double -> Int -> (Double -> Double) -> Maybe Double
trapezoidal a b n f | n <= 0 = Nothing -- n should be non-zero positive.
                    | a >= b = Nothing -- interval [a,b] should have limits such as a < b.
                    | otherwise = Just( trapezoidal' a b (fromIntegral n) ((f a)+(f b)) ((b-a)/(fromIntegral n)) (a+((b-a)/(fromIntegral n))) f ) 

simpson' :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> Double -- auxiliary function, for recursion
simpson' a b n mul area step x f | x < b = simpson' a b n (if mul == 4.0 then 2.0 else 4.0) (area+mul*(f (x))) step (x+step) f
                                 | otherwise = area * ((b-a)/(3.0*n))

simpson :: Double -> Double -> Int -> (Double -> Double) -> Maybe Double
simpson a b n f | n <= 0 = Nothing -- n should be non-zero positive.
                    | n `mod` 2 /= 0 = Nothing -- n should also be an even number according to Simpson's Rule
                    | a >= b = Nothing -- interval [a,b] should have limits such as a < b.
                    | otherwise = Just( simpson' a b (fromIntegral n) 4.0 ((f a)+(f b)) ((b-a)/(fromIntegral n)) (a+((b-a)/(fromIntegral n))) f )



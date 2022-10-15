import Data.List

polly :: Double -> Double -> Double -> Double -> Double
polly a b c x = a * x * x + b * x + c 

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n - 1) + tribonacci(n - 2) + tribonacci(n - 3)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)
-- binomial n k
    -- | k == 0 = 1
    -- | n == 0 = 1
    -- | otherwise = binomial n - 1 k + binomial n - 1 k - 1


verifL :: [Int] -> Bool
verifL v = even (length  v)
                

takeFinal :: [a] -> Int -> [a]
takeFinal v n = if n > length v
                then v
                else drop ( length v - n ) v
                
import Data.List
--Problema 7, lab2
--a)
myreplicate :: Int -> v -> [v]
myreplicate n ch
    | n == 0 = []
    | otherwise = myreplicate (n - 1) ch ++ [ch]

--b)
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs) = if even x 
                then sumImp xs
                else x + sumImp xs

--c)
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs) = if take 1 x == "A"
                    then length x + totalLen xs
                    else totalLen xs 

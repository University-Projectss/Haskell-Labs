import Data.List
import Data.Char 

--1
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = if reverse x == x  
                    then nrVocale xs + getVocals x
                    else nrVocale xs

getVocals :: [Char] -> Int
getVocals (x:xs) = if elem x "aeiouAEIOU" 
                    then getVocals xs + 1
                    else getVocals xs


--2
f :: Int -> [Int] -> [Int]
f n [] = []
f n (x:xs) 
    | even x = x : n : f n xs
    | otherwise = x : f n xs


--3
divizori :: Int -> [Int]
divizori n =  [i | i <- [1..n], mod n i == 0]

--4
listadiv :: [Int] -> [[Int]]
listadiv l = [ divizori i | i <- l ]
-- listadiv l = [ [j | j <- [1..i], mod i j == 0] | i <- l ]

--5 a)
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (x:xs)
    | elem x [a..b] = x : inIntervalRec a b xs
    | otherwise = inIntervalRec a b xs


--5 b)
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [ i | i <- l, elem i [a..b] ]

--6 a)
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) 
    | x > 0 = pozitiveRec xs + 1
    | otherwise = pozitiveRec xs

--6 b)
pozitiveComp :: [Int] -> Int
pozitiveComp l = sum [ 1 | i <- l, i > 0 ]

--7 a)
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec l = myRec 0 l

myRec :: Int -> [Int] -> [Int]
myRec _ [] = []
myRec i (x:xs) 
    | odd x = i : myRec (i + 1) xs
    | otherwise = myRec (i + 1) xs

--7 b)
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ fst i | i <- zip [0..(length l) - 1] l, even (snd i) == False ]


--8 a)
multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) 
    | digitToInt x >= 10 = multDigitsRec xs
    | otherwise = digitToInt x * (multDigitsRec xs)
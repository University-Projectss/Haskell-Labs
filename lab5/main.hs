-- 1
sumOdd :: [Int] -> Int
sumOdd l = foldl (+) 0 (map (^ 2) (filter odd l))

-- 2
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- 3
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l
  | length (filter f l) == length l = True
  | otherwise = False

allVerifiesFold :: (Int -> Bool) -> [Int] -> Bool
allVerifiesFold f l = foldr (&&) True (foldr (\x xs -> f x : xs) [] l)

-- 4
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l
  | length (filter f l) > 0 = True
  | otherwise = False

anyVerifiesFold :: (Int -> Bool) -> [Int] -> Bool
anyVerifiesFold f l = foldr (+) 0 (foldr (\x xs -> if f x then 1 : xs else 0 : xs) [] l) > 0

-- 5
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr _ [] = []
mapFoldr f l = foldr (\x xs -> f x : xs) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr _ [] = []
filterFoldr f l = foldr (\x xs -> if f x then x : xs else xs) [] l

-- 6
listToInt :: [Integer] -> Integer
listToInt l = foldl (\a b -> a * 10 + b) 0 l

-- 7
-- a
rmChar :: Char -> String -> String
rmChar ch = filter (/= ch)

-- b
rmCharsRec :: String -> String -> String
rmCharsRec [] l = l
rmCharsRec (h : t) s2 = rmCharsRec t (rmChar h s2)

-- c
rmCharsFold :: String -> String -> String
rmCharsFold s1 = foldr (\x xs -> if x `elem` s1 then x : xs else xs) []

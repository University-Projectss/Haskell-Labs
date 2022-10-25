-- 5
firstEl :: [(a, b)] -> [a]
firstEl = map fst

-- 6
sumList :: [[Int]] -> [Int]
sumList = map sum

-- 7
prel2 :: [Int] -> [Int]
prel2 = map (\x -> if even x then x `div` 2 else x * 2)

-- 8
chContain :: Char -> [String] -> [String]
chContain ch = filter (elem ch)

-- 9
oddSquare :: [Int] -> [Int]
oddSquare l = map (^ 2) (filter odd l)

-- 10
oddPozSquare :: [Int] -> [Int]
oddPozSquare l = map (\(a, b) -> a ^ 2) (filter (\(x, y) -> odd y) (zip l [0 ..]))

-- 11
numaiVocale :: [String] -> [String]
numaiVocale = map (filter (`elem` "aeiouAEIOU"))

-- 12
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (h : t) = f h : mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (h : t)
  | f h = h : myfilter f t
  | otherwise = myfilter f t

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 l1 l2 l3 = [(x, y, z) | ((x, y), z) <- zip (zip l1 l2) l3]

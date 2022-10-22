semiPare :: [Integer] -> [Integer]
semiPare [] = []
semiPare (h : t)
  | odd h = semiPare t
  | otherwise = div h 2 : semiPare t

-- Accesare lista prin index:
--     list !! i <-> list[i]
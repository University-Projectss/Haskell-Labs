semiPare :: [Integer] -> [Integer]
semiPare [] = []
semiPare (h : t)
  | odd h = semiPare t
  | otherwise = div h 2 : semiPare t

-- Accesare lista prin index:
-- list !! i <-> list[i]

-- trebuie specificat tipul lui a ca sa putem aplica
-- operatia + pe el; daca a ar fi fost un struct?
f :: (Num a) => a -> a
f x = x + 1

g :: [ceva] -> [ceva]
g (h1 : h2 : t) = t -- putem evidentia cate elemente vrem din capatul listei

-- f x y = x + y
-- f x = \y -> x + y

-- fmap este echivalent in notatie cu <$>
-- Functor == generalizare a functiei map pe orice tip de date
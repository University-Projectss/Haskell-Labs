import Distribution.SPDX (LicenseId (DOC))

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

-- Citire de la tastatura

ioString = do
  str <- getLine
  putStr str

-- in str avem un string

ioSpecific = do
  var <- readLn :: IO Double -- citim explicit un anumit tip de data
  print var

-- Citire fisiere text

inFILE = do
  var <- readFile "filePath"
  writeFile "filePath" var

-- functie care afiseaza trb sa returneze ... -> IO ()
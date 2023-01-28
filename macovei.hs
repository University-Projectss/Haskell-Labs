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

-- (>>) :: Monad m => m a -> m b -> m b
-- k >> f = k >>= \_ -> f

-- a >> b >> c >> d in varianta do:
-- do r1 <- a
--    r2 <- b r1
--    r3 <- c r2
--    d r3

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Main.Functor m => Applicative m where
  pure :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b

class Main.Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
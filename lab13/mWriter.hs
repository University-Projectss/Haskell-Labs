--- Monada Writer

newtype WriterS a = Writer {runWriter :: (a, String)}

instance Monad WriterS where
  return va = Writer (va, "")
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterS ()
tell log = Writer ((), log)

logIncrement :: Int -> WriterS Int
logIncrement x = do
  tell ("increment:" ++ show x ++ "\n")
  return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
  y <- logIncrement x
  logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
  if n > 0
    then do
      a <- logIncrement x
      logIncrementN a (n - 1)
    else return x

data Person = Person {name :: String, age :: Int}

showPersonN :: Person -> String
showPersonN (Person name age) = "NAME:" ++ show name

showPersonA :: Person -> String
showPersonA (Person name age) = "AGE:" ++ show age

showPerson :: Person -> String
showPerson (Person name age) = showPersonN (Person name age) ++ ", " ++ showPersonA (Person name age)

newtype Reader env a = Reader {runReader :: env -> a}

instance Monad (Reader env) where
  return :: a -> Reader env a
  return x = Reader (\_ -> x)

  (>>=) :: Reader env a -> (a -> Reader env b) -> Reader env b
  ma >>= k = Reader f
    where
      f env =
        let a = runReader ma env
         in runReader (k a) env

instance Applicative (Reader env) where
  pure :: a -> Reader env a
  pure = return

  (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap :: (a -> b) -> Reader env a -> Reader env b
  fmap f ma = pure f <*> ma

ask :: Reader env env
ask = Reader id

mshowPersonN :: Reader Person String
mshowPersonN = do
  env <- ask
  return ("NAME: " ++ name env)

mshowPersonA :: Reader Person String
mshowPersonA = do
  env <- ask
  return ("AGE: " ++ show (age env))

mshowPerson :: Reader Person String
mshowPerson = do
  env <- ask
  return ("(NAME: " ++ name env ++ ",AGE: " ++ show (age env) ++ ")")
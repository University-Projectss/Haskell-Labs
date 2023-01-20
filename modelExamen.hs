-- 1
data Point = Pt [Int]
  deriving (Show)

data Arb = Empty | Node Int Arb Arb
  deriving (Show)

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

getPunctToList :: Point -> [Int]
getPunctToList (Pt []) = []
getPunctToList (Pt (x : xs)) = x : getPunctToList (Pt xs)

instance ToFromArb Point where
  -- apelare: fromArb (Node 3 Empty Empty) :: Point
  fromArb Empty = Pt []
  fromArb (Node a Empty Empty) = Pt [a]
  fromArb (Node a st dr) = Pt (getPunctToList (fromArb st) ++ [a] ++ getPunctToList (fromArb dr))

  toArb (Pt []) = Empty
  toArb (Pt [a]) = Node a Empty Empty
  toArb (Pt l)
    | length l == 1 = Node (head l) Empty Empty
    | otherwise =
        if head l < (l !! 1)
          then Node (l !! 1) (Node (head l) Empty Empty) (toArb (Pt (tail (tail l))))
          else Node (head l) (Node (l !! 1) Empty Empty) (toArb (Pt (tail (tail l))))

-- 2
-- a
getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b l = [x | x <- l, a <= x && x <= b]

-- b
getFromIntervalM :: Int -> Int -> [Int] -> [Int]
getFromIntervalM a b l = do
  x <- l
  if a <= x && x <= b then return x else []

-- 3
newtype ReaderWriter env a = RW {getRW :: env -> (a, String)}

instance Monad (ReaderWriter env) where
  ma >>= k = RW f
    where
      f env =
        let (a, log) = getRW ma env
         in getRW (k a) env

instance Applicative (ReaderWriter env) where
  pure :: a -> ReaderWriter env a
  pure = return

(<*>) :: ReaderWriter env (a -> b) -> ReaderWriter env a -> ReaderWriter env b
mf <*> ma = do
  f <- mf
  a <- ma
  return (f a)

instance Functor (ReaderWriter env) where
  fmap :: (a -> b) -> ReaderWriter env a -> ReaderWriter env b
  fmap f ma = f <$> ma
import Data.Char

-- 1
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
  deriving (Show)

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

-- a
instance Show Punct where
  show (Pt []) = "()"
  show (Pt l) =
    "("
      ++ show (head l)
      ++ concat
        [ b : show a
          | (a, b) <- zip (tail l) [',', ',' ..]
        ]
      ++ ")"

-- b
getPunctToList :: Punct -> [Int]
getPunctToList (Pt []) = []
getPunctToList (Pt (x : xs)) = x : getPunctToList (Pt xs)

instance ToFromArb Punct where
  fromArb Vid = Pt []
  fromArb (F a) = Pt [a]
  fromArb (N st dr) = Pt (getPunctToList (fromArb st) ++ getPunctToList (fromArb dr))

  toArb (Pt []) = Vid
  toArb (Pt [a]) = F a
  toArb (Pt (x : xs)) = N (F x) (toArb (Pt xs))

-- 2
data Geo a = Square a | Rectangle a a | Circle a
  deriving (Show)

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) => g a -> a

-- a
instance GeoOps Geo where
  perimeter (Square l) = 4 * l
  perimeter (Rectangle lMic lMare) = 2 * lMic + 2 * lMare
  perimeter (Circle r) = 2 * pi * r

  area (Square l) = l ^ 2
  area (Rectangle lMic lMare) = lMic * lMare
  area (Circle r) = pi * (r ^ 2)

-- b
instance (Floating l, Eq l) => Eq (Geo l) where
  a == b = perimeter a == perimeter b
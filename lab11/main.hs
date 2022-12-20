-- 1
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

myConcat :: List a -> List a -> List a
myConcat a Nil = a
myConcat Nil a = a
myConcat (Cons a l) b = Cons a (myConcat l b)

instance Applicative List where
  pure a = Cons a Nil
  f <*> Nil = Nil
  Nil <*> f = Nil
  Cons f fs <*> (Cons a l) = Cons (f a) (myConcat (fmap f l) (fs <*> Cons a l))

-- 2
data Cow = Cow
  { name :: String,
    age :: Int,
    weight :: Int
  }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty s
  | not (null s) = Just s
  | otherwise = Nothing

noNegative :: Int -> Maybe Int
noNegative n
  | n < 0 = Nothing
  | otherwise = Just n

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w
  | noEmpty n == Just n && noNegative a == Just a && noNegative w == Just w = Just Cow {name = n, age = a, weight = w}
  | otherwise = Nothing

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w

-- 3
newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
  deriving (Eq, Show)

-- a
validateLength :: Int -> String -> Maybe String
validateLength n sir
  | n > length sir = Just sir
  | otherwise = Nothing

-- b
mkName :: String -> Maybe Name
mkName nume
  | validateLength 26 nume == Just nume = Just (Name nume)
  | otherwise = Nothing

mkAddress :: String -> Maybe Address
mkAddress adr
  | validateLength 101 adr == Just adr = Just (Address adr)
  | otherwise = Nothing

-- c
mkPerson :: String -> String -> Maybe Person
mkPerson n a
  | mkName n == Just (Name n) && mkAddress a == Just (Address a) = Just (Person (Name n) (Address a))
  | otherwise = Nothing

-- d
mkPerson2 :: String -> String -> Maybe Person
mkPerson2 n a = Person <$> Just (Name n) <*> Just (Address a)

mkAddress2 :: String -> Maybe Address
mkAddress2 adr = Address <$> adr
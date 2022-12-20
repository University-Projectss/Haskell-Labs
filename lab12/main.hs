import Data.Monoid

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x = foldr (\h t -> h == x || t) False

elem1' x xs = getAny $ foldMap (\h -> Any (h == x)) xs

null1 :: (Foldable t) => t a -> Bool
null1 = foldr (\x xs -> False) True

null1' xs = not (getAny $ foldMap (\x -> Any True) xs)

length1 :: (Foldable t) => t a -> Int
length1 = foldr (const (1 +)) 0

length1' xs = getSum $ foldMap (const 1) xs

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldMap (: [])

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
  foldMap f NoGoat = mempty
  foldMap f (OneGoat a) = f a
  foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c

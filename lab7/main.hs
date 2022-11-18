import Control.Arrow (ArrowChoice (right))

data Expr
  = Const Int -- integer constant
  | Expr :+: Expr -- addition
  | Expr :*: Expr -- multiplication
  deriving (Eq)

data Operation = Add | Mult deriving (Eq, Show)

data Tree
  = Lf Int -- leaf
  | Node Operation Tree Tree -- branch
  deriving (Eq, Show)

-- 1.1
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))

exp2 = (Const 2 :*: (Const 3 :+: Const 4))

exp3 = (Const 4 :+: (Const 3 :*: Const 3))

exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

instance Show Expr where
  show (Const x) = show x
  show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- 1.2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (a :+: b) = evalExp a + evalExp b
evalExp (a :*: b) = evalExp a * evalExp b

-- 1.3
arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5))

arb2 = Node Mult (Lf 2) (Node Add (Lf 3) (Lf 4))

arb3 = Node Add (Lf 4) (Node Mult (Lf 3) (Lf 3))

arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3) (Lf 1))) (Lf 2)

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add a b) = evalArb a + evalArb b
evalArb (Node Mult a b) = evalArb a * evalArb b

-- 1.4
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (a :+: b) = Node Add (expToArb a) (expToArb b)
expToArb (a :*: b) = Node Mult (expToArb a) (expToArb b)

-- 2.1
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key, value)] -> c key value

  -- a
  keys c = map fst (toList c)

  -- b
  values c = map snd (toList c)

  -- c
  fromList = foldr (uncurry insert) empty

-- 2.2
newtype PairList k v = PairList {getPairList :: [(k, v)]}

instance Collection PairList where
  empty = PairList []
  singleton key value = PairList [(key, value)]
  insert key value (PairList l) =
    if key `elem` map fst l
      then insert key value (delete key (PairList l))
      else PairList ((key, value) : l)
  toList = getPairList
  clookup key (PairList l) = Main.clookup key (PairList l)
  delete key (PairList l) = PairList $ filter (\(k, v) -> k /= key) l

-- 2.3
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key -- cheia elementului
      (Maybe value) -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k value = BNode Empty k (Just value) Empty
  insert k v Empty = singleton k v
  insert k v (BNode leftTree key val rightTree)
    | key < k = BNode leftTree key val (insert k v rightTree)
    | key > k = BNode (insert k v leftTree) key val rightTree
    | otherwise = BNode leftTree key val rightTree
  toList Empty = []
  toList (BNode leftTree key Nothing rightTree) = toList leftTree ++ toList rightTree
  toList (BNode leftTree key val rightTree) = (key, case val of { Just a -> a }) : toList leftTree ++ toList rightTree
  clookup k Empty = Nothing
  clookup k (BNode leftTree key val rightTree)
    | k == key = val
    | k < key = clookup k leftTree
    | otherwise = clookup k rightTree
  delete k (BNode leftTree key val rightTree)
    | k == key = BNode leftTree key Nothing rightTree
    | k < key = delete k leftTree
    | otherwise = delete k rightTree

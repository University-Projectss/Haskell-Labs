import Data.List
import Data.Maybe

type Nume = String

data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq)

infixr 2 :|:

infixr 3 :&:

-- 1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 =
  (Var "P" :&: (Var "Q" :|: Var "R"))
    :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

-- 2
instance Show Prop where
  show (Var a) = a
  show F = "F"
  show T = "T"
  show (Not ceva) = "(~" ++ show ceva ++ ")"
  show (a :|: b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (a :&: b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (a :->: b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (a :<->: b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup a = fromJust . lookup a

-- 3
eval :: Prop -> Env -> Bool
eval F _ = False
eval T _ = True
eval (Not a) e = not (eval a e)
eval (Var p) e = impureLookup p e
eval (a :|: b) e = eval a e || eval b e
eval (a :&: b) e = eval a e && eval b e
eval (a :->: b) e = not (eval a e) || eval b e
eval (a :<->: b) e = eval (a :->: b) e && eval (b :->: a) e

-- 4
variabile :: Prop -> [Nume]
variabile (Var p) = [p]
variabile F = []
variabile T = []
variabile (Not form) = variabile form
variabile (form1 :&: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :|: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :->: form2) = nub (variabile form1 ++ variabile form2)
variabile (form1 :<->: form2) = nub (variabile form1 ++ variabile form2)

-- 5
envs :: [Nume] -> [Env]
envs l = [zip l e | e <- sequence (take (length l) (repeat [False, True]))]

-- 6
satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p env | env <- envs (variabile p)]

-- 7
valida :: Prop -> Bool
valida p = and [eval p env | env <- envs (variabile p)]

-- 9 Facut

-- 10
echivalenta :: Prop -> Prop -> Bool
echivalenta prop1 prop2 = valida (prop1 :<->: prop2)



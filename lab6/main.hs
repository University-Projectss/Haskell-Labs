-- 1
data Fruct = Mar String Bool | Portocala String Int

-- a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar tip viermi) = False
ePortocalaDeSicilia (Portocala tip felii) = tip `elem` ["Tarocco", "Moro", "Sanguinello"]

-- b)
listaFructe =
  [ Mar "Ionatan" False,
    Portocala "Sanguinello" 10,
    Portocala "Valencia" 22,
    Mar "Golden Delicious" True,
    Portocala "Sanguinello" 15,
    Portocala "Moro" 12,
    Portocala "Tarocco" 3,
    Portocala "Moro" 12,
    Portocala "Valencia" 2,
    Mar "Golden Delicious" False,
    Mar "Golden" False,
    Mar "Golden" True
  ]

getFellii :: Fruct -> Int
getFellii (Mar _ _) = 0
getFellii (Portocala _ f) = f

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x : xs) =
  if ePortocalaDeSicilia x
    then getFellii x + nrFeliiSicilia xs
    else nrFeliiSicilia xs

-- c)
hasViermi :: Fruct -> Bool
hasViermi (Portocala _ _) = False
hasViermi (Mar _ v) = v

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x : xs) = if hasViermi x then 1 + nrMereViermi xs else nrMereViermi xs

-- 2
type NumeA = String

type Rasa = String

data Animal = Pisica NumeA | Caine NumeA Rasa
  deriving (Show)

-- a)
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meoow!"
vorbeste (Caine _ _) = "Woof!"

-- b)
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ rasa) = Just rasa

-- 3
data Linie = L [Int]
  deriving (Show)

data Matrice = M [Linie]
  deriving (Show)

-- a)

matriceaMea = M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]

-- getSumaPeLinie :: Linie -> Int
-- getSumaPeLinie (L []) = 0
-- getSumaPeLinie (L list) = sum list

getSumaLinii :: Matrice -> [Int]
getSumaLinii (M []) = []
getSumaLinii (M (L x : xs)) = sum x : getSumaLinii (M xs)

verifica :: Matrice -> Int -> Bool
verifica (M m) n = and (foldr (\x y -> if x == n then True : y else False : y) [] (getSumaLinii (M m)))

-- b
getLiniiDeLungimeN :: Matrice -> Int -> Matrice
getLiniiDeLungimeN (M m) n = M (filter (\(L l) -> length l == n) m)

getCateNegativePeLinii :: Matrice -> [Int]
getCateNegativePeLinii (M m) = map (\(L l) -> length (filter (<= 0) l)) m

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = sum (getCateNegativePeLinii (getLiniiDeLungimeN (M m) n)) == 0

-- c)

-- getLungimiEgale :: Matrice -> Either Yes No
-- getLungimiEgale (M []) = 0
-- getLungimiEgale (M (L x : xs)) = if length x == getLungimiEgale (M xs) then  else

-- corect :: Matrice -> Bool
-- corect (M m) = getLungimiEgale (M m) == 1
{- 
    1. Kreirati data tip, koji kao moguce vrednosti moze biti ili prazan
    element ili Cvor koji sadrzi informaciju i referencu na sledeci
    element u listi.
-}
data Element a = Empty
    | Cvor a (Element a) deriving Show

{- 
    2. Napisati funkciju "kreirajMojuListu" koja prima listu brojeva
    i od njenih elemenata kreira listu pomocu gore definisanog
    tipa podataka.
-}
kreirajMojuListu :: [Int] -> Element Int
kreirajMojuListu = foldr Cvor Empty

{- 
    3. Napisati funkciju "duzinaListe" koja prima gore kreiranu listu
    i vrati njenu duzinu.
-}
duzinaListe :: Element a -> Int
duzinaListe Empty = 0
duzinaListe (Cvor x sledeci) = 1 + duzinaListe sledeci

{- 
    4. Napisati funkciju "uListi" koja prima element i gore definisanu
    listu, te vraca Boolean u zavisnosti od toga da li se element
    nalazi u listi ili ne. 
-}
uListi :: Eq a => a -> Element a -> Bool
uListi x Empty = False
uListi x (Cvor y sledeci) = x == y || uListi x sledeci

{- 
    5. Definisati data tip Planeta, koji moze imati vrednosti Nista
    ili slog koji sadrzi polja za ime :: String, precnik :: Double
    i gasovita :: Boolean.
-}
data Planeta = Nista
     | Planeta {
         ime :: String,
         precnik :: Double,
         gasovita :: Bool
     } deriving Show

-- 6. Definisati tip podataka Planete kao listu planeta.
type Planete = [Planeta]

{- 
    7. Napisati funkciju "nadjiPoImenu" koja prima String i Planete i 
    vraca planetu sa datim imenom. U slucaju da je ne nadje, vraca
    Nista (definisano u data tipu Planeta).
-}
nadjiPoImenu :: String -> Planete -> Planeta
nadjiPoImenu _ [] = Nista
nadjiPoImenu imePlanete (x:xs) 
 | imePlanete == ime x = x
 | otherwise = nadjiPoImenu imePlanete xs
    

{-
    8. Napisati funkciju "vratiGasovite" koja prima Planete i vraca
    Planete, ali samo one koje su gasovite.
-}
vratiGasovite :: Planete -> Planete
vratiGasovite [] = []
vratiGasovite (x:xs)
 | gasovita x = x : vratiGasovite xs
 | otherwise  = vratiGasovite xs
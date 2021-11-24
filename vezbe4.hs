{- 
    1. Napisati funkciju koja prihvata String i razdvoji ga
    po nekom karakteru. 
    rastaviString :: Char -> String -> [String]
    
    primer: "ana voli milovana" -> "ana", "voli", "milovana"
-}
rastaviString :: Char -> String -> [String]
rastaviString delimiter string = rastaviPom string delimiter []
    where
        rastaviPom :: String -> Char -> String -> [String]
        rastaviPom [] _ acc = [acc]
        rastaviPom (x:xs) delimiter acc
            | x == delimiter = [acc] ++ rastaviPom xs delimiter []
            | otherwise      = rastaviPom xs delimiter (acc ++ [x])

{- 
    2. Napisati funkciju koja prihvata listu Stringova.
    sve stringove spoji tako sto izmedju svaka 2 
    umetne karakter ','.

    ["ana", "voli", "milovana"] + ',' -> "ana,voli,milovana"
-}
spojiStringove :: [String] -> String
spojiStringove = foldr spoji ""
    where
        spoji [] b = b
        spoji a [] = a
        spoji a b = a ++ "," ++ b

{-
    3. Napisati funkciju koja privhata listu Stringova, 
    razdvoji svaki po razmaku, te ih sve
    spoji zarezima. Koristiti map, fold i prethodne 2 funkcije.

    ["ana voli milovana", "trava je zelena"]
    -> ["ana", "voli", "milovana", "trava", "je", "zelena"]
    -> "ana,voli,milovana,trava,je,zelena"
-}
rastaviSveStringove :: [String] -> String
rastaviSveStringove [] = ""
rastaviSveStringove l = spojiStringove rastavljeni
    where
        rastavljeni = foldl (++) [] (map (rastaviString ' ') l)

{- 
    4. Napisati funkciju koja prihvata listu listi integer-a ( [[Int]] ).
    Potrebno je prvo kvadrirati elemente svake podliste, zatim ih sumirati.
    Na kraju potrebno je vratiti proizvod te liste.
    svastaSaListom :: Num a => [[a]] -> a
    
    [[1, -4, 5], [4, 4, 4], [-4, -6, -2]] -> 112896
-}
svastaSaListom :: Num a => [[a]] -> a
svastaSaListom l = foldl (*) 1 sume
    where
        kvadrirani = map (\x -> map (\y -> y ^ 2) x) l
        sume = map sum kvadrirani


{- 
    5. Definisati tip podataka naselje. Naselje moze da bude Selo, Varosica ili Grad.
    Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double). 
    Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string). Grad 
    ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).
-}
data Naselje =
    Selo {
        povrsina :: Double,
        brojStanovnika :: Int,
        tip :: String
    }
    | Varosica {
        povrsina :: Double,
        brojStanovnika :: Int
    }
    | Grad {
        povrsina :: Double,
        brojStanovnika :: Int,
        imaBazen :: Bool
    } deriving Show


{- 
    6. Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
    gradove sa bazenima koji imaju vise od 150 000 stanovnika.
-}
type Naselja = [Naselje]

izdvojiOdgovarajuce :: Naselja -> Naselja
izdvojiOdgovarajuce [] = []
izdvojiOdgovarajuce (x:xs)
    | jeSelo x && tip x == "razbijeno" = x : izdvojiOdgovarajuce xs
    | jeGrad x && imaBazen x && brojStanovnika x >= 150000 = x : izdvojiOdgovarajuce xs
    | otherwise = izdvojiOdgovarajuce xs
    where
        jeSelo (Selo {}) = True
        jeSelo _ = False
        jeGrad (Grad {}) = True
        jeGrad _ = False

import Data.Char

{- 
    1. Definisati funkciju koja prima listu brojeva i ukoliko je
    ona parne duzine, svaki element kvadrira, inace svaki element
    mnozi sa 10. Koristiti funkciju map i lambda funkcije.
-}
kvadrirajIliMnozi :: [Int] -> [Int]
kvadrirajIliMnozi [] = []
kvadrirajIliMnozi l
 | even (length l) = map (\x -> x * x) l
 | otherwise       = map (* 10) l

{- 
    2. Napisati funkciju koja prima niz karaktera. Prvo je potrebno
    funkcijom filter izbaciti sva velika slova, a zatim pomocu funkcije
    map pretvoriti sva preostala slova u velika.
    Napomena: Potrebno je u .hs fajl importovati funkciju toUpper
    iz modula Data.Char, na sledeci nacin:
    import Data.Char -- Dodati na pocetak fajla
-}
pretvoriSlova :: [Char] -> [Char]
pretvoriSlova [] = []
pretvoriSlova l = map toUpper mala
    where 
        mala = filter isLower l

{- 
    3. Napisati funkciju "primeni" koja prima 3 parametra:
    1) f1 :: [[Int]] -> [[Int]] - funkciju koja prihvata [[Int]] i vraca [[Int]]
    2) f2 :: [[Int]] -> [Int] - funkciju koja prihvata [[Int]] i vraca [Int]
    3) listu ciji su elementi liste brojeva, tj. [[Int]]
    Funkcija treba da primeni funkcije f1 i f2 nad prosledjenom listom
    i vrati rezultat (listu tipa [[Int]] pretvara u [Int], pomocu 
    funkcija f1 i f2).
-}
primeni :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
primeni _ _ [] = []
primeni f1 f2 l = f2 $ f1 l


{-
    4. Definisati funkciju "izbaciParnePaSumiraj" preko funkcije "primeni"
    i funkcija iz zadataka 2 i 3 (sa Vezbi 2), koristeci parcijalnu 
    primenu funkcija.
-}
sumeListi :: [[Int]] -> [Int]
sumeListi = map sum

izbaciParne :: [[Int]] -> [[Int]]
izbaciParne l = map (filter odd) (filter (any odd) l)

izbaciParnePaSumiraj = primeni izbaciParne sumeListi

{- 
    5. Definisati funkciju prosecnaDuzina koja prima niz stringova
    i racuna njihovu prosecnu duzinu pomocu funkcije fold i map.
    Dovoljno je vratiti celobrojnu vrednost (npr. 5 umesto 5.3)
-}
prosecnaDuzina :: [String] -> Int 
prosecnaDuzina [] = 0
prosecnaDuzina l = div suma (length l)
    where
        suma = foldl (+) 0 (map length l) -- alternativa sum (map length l)
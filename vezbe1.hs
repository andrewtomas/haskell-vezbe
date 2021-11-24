import Data.Char

-- 1. Napisati funkciju koja uklanja poslednji element liste. 
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

-- 2. Napisati funkciju koja uklanja pretposlednji element liste.
removeSecondToLast [] = []
removeSecondToLast [x] = [x]
removeSecondToLast (x:y:xs) = x:xs

-- 3. Izracunati n! (a) rekurzivno (b) repno rekurzivno
fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact n

fact' n = factAcc n 1
factAcc n acc
 | n == 0    = acc
 | otherwise = factAcc (n - 1) (acc * n)

{- 
  4. Napisati funkciju "imaVelikaSlova" koja proverava 
     da li neki string ima velika slova
-}
imaVelikaSlova :: [Char] -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (x:xs)
 | isAsciiUpper x = True
 | otherwise            = imaVelikaSlova xs


-- 5. Napisati funkciju "spljosti".
spljosti [] = []
spljosti [x] = [x]
spljosti (x:y:t)
 | x == y = spljosti (x:t)
 | otherwise = x : spljosti (y:t)

-- 6. Napisati funkciju koja iz liste stringova izbacuje one koji imaju sva mala slova
izbaciSamoMala :: [[Char]] -> [[Char]]
izbaciSamoMala [] = []
izbaciSamoMala (x:xs)
        | imaVelikaSlova x = x : izbaciSamoMala xs
        | otherwise        = izbaciSamoMala xs

-- 7. Napisati funkciju koja kvadrira sve elemente liste (preko ZF izraza).
kvadrirajListu :: [Int] -> [Int]
kvadrirajListu l = [x * x | x <- l]

-- 8. Napisati funkciju jeDeljiv, koji prima 2 broja i vraca true ako prvi broj moze da deli drugi.
jeDeljiv :: Int -> Int -> Bool
jeDeljiv 0 _ = False
jeDeljiv a b = mod b a == 0

-- 9. Napisati funkciju jeDeljivSa3 koristeci prethodno definisanu funkciju
jeDeljivSa3 = jeDeljiv 3

-- 10. Napisati funkciju filter' f l, koja filtrira elemente liste l pomocu funkcije f.
filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' f [] = []
filter' f (x:xs)
        | f x       = x : filter' f xs
        | otherwise = filter' f xs

-- 11. Definisati funkciju deljiviSa3 koristeci prethodne 2 funkcije.
deljiviSa3 :: [Int] -> [Int]
deljiviSa3 = filter' jeDeljivSa3

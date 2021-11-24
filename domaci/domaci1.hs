-- 1. Napisati funkciju koja uklanja svaki N-ti element liste.
removeNth l n = removeNthHelp l n 0
removeNthHelp [] _ _ = []
removeNthHelp (x:xs) n k
 | n == k + 1    = removeNthHelp xs n 0
 | otherwise = x : removeNthHelp xs n (k + 1)

-- 2. Napisati funkciju koja vraca listu delilaca nekog pozitivnog broja.
delioci :: Int -> [Int]
delioci 0 = []
delioci n = [x | x <- [1..n], mod x n == 0]

-- 3. Napisati quicksort.
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (manji xs) ++ [x] ++ quicksort (veci xs)
 where
    manji = filter (<= x)
    veci = filter (> x)

-- 4 Definisati filter'' f l preko ZF izraza
filter'' :: (Int -> Bool) -> [Int] -> [Int]
filter'' f l = [x | x <- l, f x]

-- 5. Napisati funkciju koja sumira sve elemente liste.
sumListu :: [Int] -> Int
sumListu = sum

-- 6. Napisati funkciju zip' koja prima 2 liste i sabira 
-- elemente na istim indeksima.
zip' :: [Int] -> [Int] -> [Int]
zip' [] [] = []
zip' l1 [] = l1
zip' [] l2 = l2
zip' (h1:t1) (h2:t2) = (h1 + h2) : zip' t1 t2

{- 
    7. Napisati funkciju koja sumira sve cifre prosledjenog broja:
    a. rekurzivno
    b. repno rekurzivno
-}
sumCif :: Int -> Int
sumCif 0 = 0
sumCif n = mod n 10 + sumCif (n `div` 10)

sumaCifara' :: Int -> Int
sumaCifara' n = sumaCifaraAcc n 0

sumaCifaraAcc :: Int -> Int -> Int
sumaCifaraAcc 0 acc = acc
sumaCifaraAcc n acc = sumaCifaraAcc (n `div` 10) (acc + mod n 10)

-- 8. Sumirati parne cifre nekog broja 
sumParCif :: Int -> Int
sumParCif 0 = 0
sumParCif n
    | even cif  = cif + sumParCif sled
    | otherwise = sumParCif sled
    where
        cif  = mod n 10
        sled = n `div` 10

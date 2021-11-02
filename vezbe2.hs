-- 1. Napisati funkciju "ispeglaj" koja prima listu ciji su elementi
--    liste [[Int]], a vraca listu brojeva [Int] tako sto sve elemente
--    podlisti spoji u jednu listu.
--    [[1, 2, 3], [4, 5], [6], [7, 8], [], [9]] -> [1, 2, 3, 4, 5, 6, 7, 8, 9] 
ispeglaj :: [ [Int] ] -> [Int]
ispeglaj t = foldr (++) [] t

-- 2. Napisati funkciju koja prima listu listi brojeva [[Int]], i vraca
--   listu suma elemenata listi koje ona sadrzi. Koristiti funkciju fold.
--   [[1, 2, 3], [2, 3], [1, 2, 4], [5]] -> [6, 5, 7, 5]
sumeListi :: [[Int]] -> [Int]
sumeListi t = map sum t

-- 3. Napisati funkciju koja prima listu ciji su elementi liste brojeva,
--    te iz svake podliste izbacuje parne brojeve. Ako slucajno ostane
--    neka prazna lista, nju je potrebno izbaciti. Koristiti funkciju
--    filter.
--    [[1, 2, 3], [2, 4], [3, 4, 5], [7]] -> [[1, 3], [3, 5], [7]]
izbaciParne :: [[Int]] -> [[Int]]
izbaciParne l = map (filter odd) (filter (any odd) l)

izbaciParne' :: [[Int]] -> [[Int]]
izbaciParne' [] = []
izbaciParne' (h:t)
  | null neparni = izbaciParne t
  | otherwise = neparni : izbaciParne t
  where
    neparni = filter odd h


-- 4. Napisati funkciju "okreni" koja prolazi kroz listu stringova, 
--    te pomocu funkcija map i reverse "okrene" sve stringove.
--    "neki string" -> "gnirts iken"
okreni :: [[Char]] -> [[Char]]
okreni [] = []
okreni l = map reverse l

-- 5. Napisati funkciju koja prihvata listu listi brojeva [[Int]] i 
--    iz svake podliste izbacuje elemente deljive sa 3.
izbaciDeljive3 :: [[Int]] -> [[Int]]
izbaciDeljive3 l = filter (/= []) (map (filter (\x -> mod x 3 /= 0)) l)

-- 6. Napisati funkciju koja prihvata listu listi brojeva, primenjuje na 
--    nju prethodnu funkciju i onda izbaci sve podliste koje imaju manje od 5 elemenata.
izbaciManjeOd5 :: [[Int]] -> [[Int]]
izbaciManjeOd5 l = filter (\x -> (length x) > 5) (izbaciDeljive3 l)

-- (subsecuencias xs) es la lista de las subsecuencias de xs. Por ejemplo,
--    subsecuencias "bc"   ==  ["","c","b","bc"]
--    subsecuencias "abc"  ==  ["","c","b","bc","a","ac","ab","abc"]
subsecuencias :: [a] -> [[a]]
subsecuencias []     = [[]]
subsecuencias (x:xs) = yss ++ map (x:) yss
    where yss = subsecuencias xs

-- Puede definirse usando la  predefinida subsequences (aunque el orden de los
-- elementos es distinto).
subsecuencias' :: [a] -> [[a]]
subsecuencias' = Data.List.subsequences

-- (intercala x ys) es la lista de las listas obtenidas intercalando x
-- entre los elementos de ys. Por ejemplo,
--    intercala 'x' "bc"  ==  ["xbc","bxc","bcx"]
--    intercala 'x' "abc"  ==  ["xabc","axbc","abxc","abcx"]
intercala :: a -> [a] -> [[a]]
intercala x []     = [[x]]
intercala x (y:ys) = (x:y:ys) : map (y:) (intercala x ys)

-- (permutaciones xs) es la lista de las permutaciones de xs. Por
-- ejemplo,
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","bca","acb","cab","cba"]
permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = concat (map (intercala x) (permutaciones xs))

-- Puede definirse usando la  predefinida permutations (aunque el orden de los
-- elementos es distinto).
permutaciones' :: [a] -> [[a]]
permutaciones' = Data.List.permutations

-- (elecciones xs) es la lista formada por todas las subsecuencias de xs en
-- cualquier orden. Por ejemplo,
--    *Main> elecciones "abc"
--    ["","c","b","bc","cb","a","ac","ca","ab","ba",
--     "abc","bac","bca","acb","cab","cba"]
elecciones :: [a] -> [[a]]
elecciones xs = concat (map permutaciones (subsecuencias xs))

-- (variaciones xs n) es la lista formada por las variaciones de los xs elementos
-- tomados de n en n. Por ejemplo,
-- *Main> variaciones [1,2,3,4] 2
-- [[3,4],[4,3],[2,4],[4,2],[2,3],[3,2],[1,4],[4,1],[1,3],[3,1],[1,2],[2,1]]
variaciones :: [a] -> [[a]]
variaciones xs n = filter ((== n) . length) (elecciones xs)

import Data.List

-- Formas de sumar un nº con otros dos tales que el primero no es menor que el
-- segundo.
sums :: Int -> [(Int, Int)]
sums n = [(n - u, u) | u <- [1..n `div` 2]]


-- Genera todas las expresiones de calculadora polaca (leyendo por la izq) de
-- operaciones binarias.
trees :: Int -> [String]
trees 1 = ["*"]
trees 2 = ["**+"]
trees n = concatMap subtrees $ sums n
              where subtrees (v, u) = [a ++ b ++ "+" | a <- trees v
                                                     , b <- trees u]

-- ---------------------------------------------------------------------
-- 2. Formalización del problema                                      --
-- ---------------------------------------------------------------------

-- Expresiones
-- -----------

-- Las operaciones son sumar, restar, multiplicar o dividir.
data Op = Sum | Res | Mul | Div
          deriving Eq

instance Show Op where
   show Sum = "+"
   show Res = "-"
   show Mul = "*"
   show Div = "/"

-- (valida o x y) se verifica si la operación o aplicada a los números
-- náturales x e y da un número natural. Por ejemplo,
--    valida Res 5 3  ==  True
--    valida Res 3 5  ==  False
--    valida Div 6 3  ==  True
--    valida Div 6 4  ==  False
valida :: Op -> Int -> Int -> Bool
valida Sum _ _ = True
valida Res x y = x > y
valida Mul _ _ = True
valida Div x y = x `mod` y == 0

-- (aplica o x y) es el resultado de aplicar la operación o a los
-- números naturales x e y. Por ejemplo,
--    aplica Sum 2 3  ==  5
--    aplica Div 6 3  ==  2
aplica :: Op -> Int -> Int -> Int
aplica Sum x y = x + y
aplica Res x y = x - y
aplica Mul x y = x * y
aplica Div x y = x `div` y

-- Otra definición de aplica es
aplica' :: Op -> Int -> Int -> Int
aplica' Sum = (+)
aplica' Res = (-)
aplica' Mul = (*)
aplica' Div = div


-- Las expresiones son números enteros o aplicaciones de operaciones a
-- dos expresiones.
data Expr = Num Int | Apl Op Expr Expr
            deriving Eq

instance Show Expr where
   show (Num n)     = show n
   show (Apl o i d) = parentesis i ++ show o ++ parentesis d
                      where
                         parentesis (Num n) = show n
                         parentesis e       = "(" ++ show e ++ ")"

-- Expresión correspondiente a (1+50)*(25−10)
ejExpr :: Expr
ejExpr =  Apl Mul e1 e2
    where e1 = Apl Sum (Num 1) (Num 50)
          e2 = Apl Res (Num 25) (Num 10)

-- (numeros e) es la lista de los números que aparecen en la expresión
-- e. Por ejemplo,
--    numeros (Apl Mul (Apl Sum (Num 2) (Num 3)) (Num 7))  ==  [2,3,7]
numeros :: Expr -> [Int]
numeros (Num n)     = [n]
numeros (Apl _ l r) = numeros l ++ numeros r

-- (valor e) es la lista formada por el valor de la expresión e si todas
-- las operaciones para calcular el valor de e son números positivos y
-- la lista vacía en caso contrario. Por ejemplo,
--    valor (Apl Mul (Apl Sum (Num 2) (Num 3)) (Num 7))  ==  [35]
--    valor (Apl Res (Apl Sum (Num 2) (Num 3)) (Num 7))  ==  []
--    valor (Apl Sum (Apl Res (Num 2) (Num 3)) (Num 7))  ==  []
valor :: Expr -> [Int]
valor (Num n)     = [n | n > 0]
valor (Apl o i d) = [aplica o x y | x <- valor i
                                  , y <- valor d
                                  , valida o x y]

-- Funciones combinatorias
-- -----------------------

-- (sublistas xs) es la lista de las sublistas de xs. Por ejemplo,
--    sublistas "bc"   ==  ["","c","b","bc"]
--    sublistas "abc"  ==  ["","c","b","bc","a","ac","ab","abc"]
sublistas :: [a] -> [[a]]
sublistas []     = [[]]
sublistas (x:xs) = yss ++ map (x:) yss
    where yss = sublistas xs

-- Puede definirse usando la  predefinida subsequences (aunque el orden de los
-- elementos es distinto).
sublistas' :: [a] -> [[a]]
sublistas' = Data.List.subsequences

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

-- (elecciones xs) es la lista formada por todas las sublistas de xs en
-- cualquier orden. Por ejemplo,
--    *Main> elecciones "abc"
--    ["","c","b","bc","cb","a","ac","ca","ab","ba",
--     "abc","bac","bca","acb","cab","cba"]
elecciones :: [a] -> [[a]]
elecciones xs = concat (map permutaciones (sublistas xs))

-- Formalización del problema
-- --------------------------

-- (solucion e ns n) se verifica si la expresión e es una solución para
-- la sucesión ns y objetivo n; es decir. si los números de e es una
-- posible elección de ns y el valor de e es n. Por ejemplo,
--    solucion ejExpr [1,3,7,10,25,50] 765  ==  True
solucion :: Expr -> [Int] -> Int -> Bool
solucion e ns n = elem (numeros e) (elecciones ns) && valor e == [n]

-- ---------------------------------------------------------------------
-- 3. Solución por fuerza bruta                                       --
-- ---------------------------------------------------------------------

-- (divisiones xs) es la lista de las divisiones de xs en dos listas no
-- vacías. Por ejemplo,
--    divisiones "bcd"   ==  [("b","cd"),("bc","d")]
--    divisiones "abcd"  ==  [("a","bcd"),("ab","cd"),("abc","d")]
divisiones :: [a] -> [([a],[a])]
divisiones []     = []
divisiones [_]    = []
divisiones (x:xs) = ([x],xs) : [(x:is,ds) | (is,ds) <- divisiones xs]

-- (expresiones ns) es la lista de todas las expresiones constructibles
-- a partir de la lista de números ns. Por ejemplo,
--   *Main> expresiones [2,3,5]
--   [2+(3+5),2-(3+5),2*(3+5),2/(3+5),2+(3-5),2-(3-5),2*(3-5),2/(3-5),
--    2+(3*5),2-(3*5),2*(3*5),2/(3*5),2+(3/5),2-(3/5),2*(3/5),2/(3/5),
--    (2+3)+5,(2+3)-5,(2+3)*5,(2+3)/5,(2-3)+5,(2-3)-5,(2-3)*5,(2-3)/5,
--    (2*3)+5,(2*3)-5,(2*3)*5,(2*3)/5,(2/3)+5,(2/3)-5,(2/3)*5,(2/3)/5]
expresiones :: [Int] -> [Expr]
expresiones []  = []
expresiones [n] = [Num n]
expresiones ns  = [e | (is,ds) <- divisiones ns
                     , i       <- expresiones is
                     , d       <- expresiones ds
                     , e       <- combina i d]

-- (combina e1 e2) es la lista de las expresiones obtenidas combinando
-- las expresiones e1 y e2 con una operación. Por ejemplo,
--    combina (Num 2) (Num 3)  ==  [2+3,2-3,2*3,2/3]
combina :: Expr -> Expr -> [Expr]
combina e1 e2 = [Apl o e1 e2 | o <- ops]

-- ops es la lista de las operaciones.
ops :: [Op]
ops = [Sum,Res,Mul,Div]

-- (soluciones ns n) es la lista de las soluciones para la sucesión ns y
-- objetivo n calculadas por fuerza bruta. Por ejemplo,
--    *Main> soluciones [1,3,7,10,25,50] 765
--    [3*((7*(50-10))-25), ((7*(50-10))-25)*3, ...
--    *Main> :set +s
--    *Main> head (soluciones [1,3,7,10,25,50] 765)
--    3*((7*(50-10))-25)
--    (8.47 secs, 400306836 bytes)
--    *Main> length (soluciones [1,3,7,10,25,50] 765)
--    780
--    (997.76 secs, 47074239120 bytes)
--    *Main> length (soluciones [1,3,7,10,25,50] 831)
--    0
--    (1019.13 secs, 47074535420 bytes)
--    *Main> :unset +s
soluciones :: [Int] -> Int -> [Expr]
soluciones ns n =  [e | ns' <- elecciones ns
                      , e   <- expresiones ns'
                      , valor e == [n]]

-- ---------------------------------------------------------------------
-- 4. Combinando generación y evaluación                              --
-- ---------------------------------------------------------------------

-- Resultado es el tipo de los pares formados por expresiones válidas y
-- su valor.
type Resultado = (Expr,Int)

-- (resultados ns) es la lista de todos los resultados constructibles
-- a partir de la lista de números ns. Por ejemplo,
--    *Main> resultados [2,3,5]
--    [(2+(3+5),10), (2*(3+5),16), (2+(3*5),17), (2*(3*5),30), ((2+3)+5,10),
--     ((2+3)*5,25), ((2+3)/5,1),  ((2*3)+5,11), ((2*3)-5,1),  ((2*3)*5,30)]
resultados :: [Int] -> [Resultado]
resultados []  = []
resultados [n] = [(Num n,n) | n > 0]
resultados ns  = [res | (is,ds) <- divisiones ns
                      , ix      <- resultados is
                      , dy      <- resultados ds
                      , res     <- combina' ix dy]

-- (combina' r1 r2) es la lista de los resultados obtenidos combinando
-- los resultados r1 y r2 con una operación. Por ejemplo,
--    combina' (Num 2,2) (Num 3,3)  ==  [(2+3,5),(2*3,6)]
--    combina' (Num 3,3) (Num 2,2)  ==  [(3+2,5),(3-2,1),(3*2,6)]
--    combina' (Num 2,2) (Num 6,6)  ==  [(2+6,8),(2*6,12)]
--    combina' (Num 6,6) (Num 2,2)  ==  [(6+2,8),(6-2,4),(6*2,12),(6/2,3)]
combina' :: Resultado -> Resultado -> [Resultado]
combina' (i,x) (d,y) =  [(Apl o i d, aplica o x y) | o <- ops
                                                   , valida o x y]

-- (soluciones' ns n) es la lista de las soluciones para la sucesión ns y
-- objetivo n calculadas intercalando generación y evaluación. Por
-- ejemplo,
--    *Main> head (soluciones' [1,3,7,10,25,50] 765)
--    3*((7*(50-10))-25)
--    (0.81 secs, 38804220 bytes)
--    *Main> length (soluciones' [1,3,7,10,25,50] 765)
--    780
--    (60.73 secs, 2932314020 bytes)
--    *Main> length (soluciones' [1,3,7,10,25,50] 831)
--    0
--    (61.68 secs, 2932303088 bytes)
soluciones' :: [Int] -> Int -> [Expr]
soluciones' ns n = [e | ns'   <- elecciones ns
                      , (e,m) <- resultados ns'
                      , m == n]

-- ---------------------------------------------------------------------
-- 5. Mejora mediante propiedades algebraicas                         --
-- ---------------------------------------------------------------------

-- (valida' o x y) se verifica si la operación o aplicada a los números
-- náturales x e y da un número natural, teniendo en cuenta las
-- siguientes reducciones aritméticas
--    x + y = y + x
--    x * y = y * x
--    x * 1 = x
--    1 * y = y
--    x / 1 = x
-- Por ejemplo,
--    valida' Sum 2 3  ==  True
--    valida' Sum 3 2  ==  False
--    valida' Res 3 2  ==  True
--    valida' Res 2 3  ==  False
--    valida' Mul 1 3  ==  False
--    valida' Mul 3 1  ==  False
--    valida' Mul 2 3  ==  True
--    valida' Mul 3 2  ==  False
--    valida' Div 3 1  ==  False
--    valida' Div 3 2  ==  False
--    valida' Div 4 2  ==  True
valida' :: Op -> Int -> Int -> Bool
valida' Sum x y = x <= y
valida' Res x y = x > y
valida' Mul x y = x /= 1 && y /= 1 && x <= y
valida' Div x y = y /= 1 && x `mod` y == 0

-- (resultados' ns) es la lista de todos los resultados válidos
-- constructibles a partir de la lista de números ns. Por ejemplo,
--    *Main> resultados [5,3,2]
--    [(5+(3+2),10), (5*(3+2),25), (5/(3+2),1), (5+(3-2),6), (5-(3-2),4),
--     (5*(3-2),5),  (5/(3-2),5),  (5+(3*2),11),(5*(3*2),30),((5+3)+2,10),
--     ((5+3)-2,6),  ((5+3)*2,16), ((5+3)/2,4), ((5-3)+2,4), ((5-3)*2,4),
--     ((5-3)/2,1),  ((5*3)+2,17), ((5*3)-2,13), ((5*3)*2,30)]
--    *Main> resultados' [5,3,2]
--    [(5-(3-2),4),((5-3)+2,4),((5-3)*2,4),((5-3)/2,1)]
resultados' :: [Int] -> [Resultado]
resultados' []  = []
resultados' [n] = [(Num n,n) | n > 0]
resultados' ns  = [res | (is,ds) <- divisiones ns
                       , ix      <- resultados' is
                       , dy      <- resultados' ds
                       , res     <- combina'' ix dy]

-- (combina'' r1 r2) es la lista de los resultados válidos obtenidos
-- combinando los resultados r1 y r2 con una operación. Por ejemplo,
--    combina'' (Num 2,2) (Num 3,3)  ==  [(2+3,5),(2*3,6)]
--    combina'' (Num 3,3) (Num 2,2)  ==  [(3-2,1)]
--    combina'' (Num 2,2) (Num 6,6)  ==  [(2+6,8),(2*6,12)]
--    combina'' (Num 6,6) (Num 2,2)  ==  [(6-2,4),(6/2,3)]
combina'' :: Resultado -> Resultado -> [Resultado]
combina'' (i,x) (d,y) =
    [(Apl o i d, aplica o x y) | o <- ops
                               , valida' o x y]

-- (soluciones'' ns n) es la lista de las soluciones para la sucesión ns y
-- objetivo n calculadas intercalando generación y evaluación y usando
-- las mejoras aritméticas. Por ejemplo,
--    *Main> head (soluciones'' [1,3,7,10,25,50] 765)
--    3*((7*(50-10))-25)
--    (0.40 secs, 16435156 bytes)
--    *Main> length (soluciones'' [1,3,7,10,25,50] 765)
--    49
--    (10.30 secs, 460253716 bytes)
--    *Main> length (soluciones'' [1,3,7,10,25,50] 831)
--    0
--    (10.26 secs, 460253908 bytes)
soluciones'' :: [Int] -> Int -> [Expr]
soluciones'' ns n = [e | ns'   <- elecciones ns
                       , (e,m) <- resultados' ns'
                       , m == n]

{-
Comparación para el problema [1,3,7,10,25,50] 765
                  +---------------------+-------------------------+
                  | Primera solución    | Todas soluciones        |
                  +-------+-------------+--------+----------------+
                  | segs. | byte  s     | segs.  | bytes          |
   +--------------+-------+-------------+--------+----------------+
   | soluciones   | 8.47  | 400.306.836 | 997.76 | 47.074.239.120 |
   | soluciones'  | 0.81  |  38.804.220 |  60.73 |  2.932.314.020 |
   | soluciones'' | 0.40  |  16.435.156 |  10.30 |    460.253.716 |
   +--------------+-------+-------------+--------+----------------+

Comparación para el problema [1,3,7,10,25,50] 831
                  +--------------------------+
                  | No tiene soluciones      |
                  +---------+----------------+
                  | segs.   | bytes          |
   +--------------+---------+----------------+
   | soluciones   | 1019.13 | 47.074.535.420 |
   | soluciones'  |   61.68 |  2.932.303.088 |
   | soluciones'' |   10.26 |    460.253.908 |
   +--------------+---------+----------------+
-}

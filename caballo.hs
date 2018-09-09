{-
    EL SALTO DEL CABALLO

    Digamos que tenemos un tablero de ajedrez y como única pieza un caballo.
    Queremos saber si el caballo puede alcanzar una determinada posición en uno
    o varios movimientos. Utilizaremos una dupla de números para representar la
    posición del caballo en el tablero. El primer número representará la
    columna en la que está el caballo y el segundo representará la fila.

    Digamos que el caballo empieza en (6,2) ¿Puede alcanzar (6,1)?

    Es un ejemplo de BÚSQUEDA EN UN ESPACIO DE ESTADOS.

    - Un NODO es cada uno de los vértices del grafo correspondiente. En este
      caso, representado por una posición en el tablero: una tupla
      (columna,fila).

    - El NODO INICIAL es aquel por el que se comienza a buscar. En nuestro
      ejemplo, es el (6,2).

    - El NODO FINAL es aquel que se pretende alcanzar. En nuestro caso, es el
      (6.1).

    - Un CAMINO es una lista de vértices, donde cada par de vértices están
      adyacentes entre sí. Por ejemplo: C -> B -> F.

    - Un ESTADO representa un camino a un determinado nodo, partiendo del nodo
      inicial. Para facilitar la implementación, lo almacenaremos al revés. Por
      ejemplo: el estado [D,C,B,A] representa el camino A -> B -> C -> D,
      suponiendo que el nodo inicial es A.

    - El ESTADO INICIAL es aquel que sólo contiene al nodo inicial. Por
      ejemplo, en nuestro caso sería [(6,2)].

    - Un ESTADO FINAL es aquel cuyo último nodo es el nodo final. Recordando
      que el estado implementa la lista al revés, significa por tanto que un
      estado final es aquel cuyo primer elemento es el nodo final. Por ejemplo:
      [(6,1),(7,3),(5,4),(6,2)].

    - La función esFinal determina si un estado es final o no.

    - La función sucesores devuelve una lista con todos los posibles estados a
      los que se puede alcanzar directamente desde un estado dado. Por ejemplo:
      sucesores [(6,2)] = [[(8,1),(6,2)]
                          ,[(8,3),(6,2)]
                          ,[(4,1),(6,2)]
                          ,[(4,3),(6,2)]
                          ,[(7,4),(6,2)]
                          ,[(5,4),(6,2)]
                          ]

    - Las funciones solProf resuelven el problema realizando una búsqueda en
      profundidad en el espacio de estados del problema.

    - Las funciones solAnch resuelven el problema realizando una búsqueda en
      anchura en el espacio de estados del problema.

    - Si hay solución, la búsqueda en anchura la encontrará y será óptima (en
      cuanto a número de pasos), pero puede quedarse sin recursos por explosión
      combinatoria.

    - La búsqueda en profundidad es más rápida que la búsqueda en anchura, pero
      puede no encontrar la solución por perderse demasiado al fondo en el
      árbol de búsqueda. Para solucionarlo, se usa una variante de la búsqueda
      acotada, que en este caso consiste simplemente en limitar la longitud
      máxima del camino que puede formar un determinado estado (ver
      `length s < 7` en la función sucesores).
-}

type Nodo = (Int,Int)       -- En este ejemplo, un nodo es una posición del tablero
type Estado = [Nodo]        -- Un estado es una lista de nodos, partiendo del inicial

esFinal :: Estado -> Bool
esFinal [] = False
esFinal (e:es) = e == (6,1)

-- Implementación alternativa:
esFinal' :: Estado -> Bool
esFinal' ((6,1):_) = True
esFinal' _         = False

-- Estados a los que se puede acceder desde uno dado. Se evitan los ciclos y se
-- limita la longitud máxima del camino correspondiente:
sucesores :: Estado -> [Estado]
sucesores [] = []
sucesores s@((c,r):ss) = [(c',r'):s | (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                                                 ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                                                 ]
                                    , c' `elem` [1..8], r' `elem` [1..8]
                                    , not ((c',r') `elem` ss)
                                    , length s < 7]

-- BÚSQUEDAS EN PROFUNDIDAD:

-- Versión recursiva con listas por comprensión sin acumulación de nodos por visitar:
solProf :: Nodo -> [Estado]
solProf n = aux [n]
    where
        aux :: Estado -> [Estado]
        aux e@(n:ns)
            | esFinal e = [reverse e]
            | otherwise = [z | s <- sucesores e, z <- aux s]

-- Versión equivalente a la anterior traduciendo la lista por comprensión usando concatMap:
solProf' :: Nodo -> [Estado]
solProf' n = aux [n]
    where
        aux :: Estado -> [Estado]
        aux e@(n:ns)
            | esFinal e = [reverse e]
            | otherwise = concatMap (\s -> concatMap (\z -> [z]) (aux s)) (sucesores e)

-- Versión equivalente a la anterior usando funciones de orden superior:
solProf'' :: Nodo -> [Estado]
solProf'' n = aux [n]
    where
        aux :: Estado -> [Estado]
        aux e@(n:ns)
            | esFinal e = [reverse e]
            | otherwise = concatMap (aux) (sucesores e)

-- Versión recursiva con listas por comprensión basada en el algoritmo original,
-- acumulando los nodos por visitar:
solProf''' :: Nodo -> [Estado]
solProf''' n = aux [[n]]
    where
        aux :: [Estado] -> [Estado]
        aux [] = []
        aux (e:es)
            | esFinal e = (reverse e):(aux es)
            | otherwise = [z | let s = sucesores e, z <- aux (s ++ es)]

-- Versión recursiva con recursividad final semiiterativa basada en el algoritmo original:
solProf'''' :: Nodo -> [Estado]
solProf'''' n = aux [[n]]
    where
        aux :: [Estado] -> [Estado]
        aux [] = []
        aux (e:es)
            | esFinal e = (reverse e):(aux es)
            | otherwise = aux ((sucesores e) ++ es)

-- BÚSQUEDAS EN ANCHURA:

-- Versión recursiva con listas por comprensión basada en el algoritmo original,
-- acumulando los nodos por visitar:
solAnch :: Nodo -> [Estado]
solAnch n = aux [[n]]
    where
        aux :: [Estado] -> [Estado]
        aux [] = []
        aux (e:es)
            | esFinal e = (reverse e):(aux es)
            | otherwise = [z | let s = sucesores e, z <- aux (es ++ s)]

-- Versión recursiva con recursividad final semiiterativa basada en el algoritmo original:
solAnch' :: Nodo -> [Estado]
solAnch' n = aux [[n]]
    where
        aux :: [Estado] -> [Estado]
        aux [] = []
        aux (e:es)
            | esFinal e = (reverse e):(aux es)
            | otherwise = aux (es ++ (sucesores e))

-- Usamos una implementación cualquiera de entre todas las anteriores:
test = solProf (6,2)

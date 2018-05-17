module RInterconexas(principalAux
  ) where

import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

--Recibe las conexiones que existentes entre los nodos
-- y retorna una lista de esas conexiones y le agrega un peso de 1
-- como es un grafo no dirigido, genera la conexion en ambas direcciones.
--  Ejemplo: construirGrafo [("a","b"),("c","a")]
--       retorna: [("a",[("b",1),("c",1)]),("b",[("a",1)]),("c",[("a","b")])]
construirGrafoAux :: Ord a => [(a, a)] -> Map a [(a, Float)]
construirGrafoAux g = fromListWith (++) $ g >>=
               \(a,b) -> [(a,[(b,1)]), (b,[(a,1)])]

-- Recibe la lista de conexiones con la forma: [A,B,C,D],
-- donde las conexiones se agrupan de dos en dos, es decir
-- A,B es una conexion y C,D es otra.
-- Toma esta lista y la convierte en la forma [(A,B),(C,D)] con la
-- funcion acomodarConexiones, y construye el grafo llamando a la
-- funcion construirGrafoAux
construirGrafo ::  Ord a => [a]-> Map a [(a, Float)]
construirGrafo lista = construirGrafoAux (acomodarConexiones (particionarListaAux lista))

-- Algoritmo Dijkstra, recibe el nodo inicial y el grafo.
-- y retorna las rutas y el costo hacia cada nodo del grafo
-- Como el costo de ir al nodo inicial es 0, se le asigna un costo de 0
-- Si no existe conexion entre dos nodos le agrega costo Infinity(1/0)
-- en otro caso el costo es 1
-- Ejemplo: se tienen las conexiones [("a","b"),("c","a"),("e","d")]
--    resultado de calcular dijkstra de "a" en el grafo que asociado
--    es el siguiente: [("a",(0.0,Nothing),("b",(1,Just "a")),("c",(1,Just "a")),
--                      ("d",(Infinity,Nothing)),("e",(Infinity,Nothing))]  

dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) 
                | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

-- Retorna el costo de la ruta más corta entre dos nodos dados.
rutaMasCorta :: Ord a => a -> a -> Map a [(a, Float)] -> Int
rutaMasCorta from to graph = length (rutaMasCortaAux from to graph) - 1

-- Retorna los nodos de la ruta más corta entre dos nodos dados.
-- Ejemplo: Se requiere conocer la ruta más corta entre "c" y "b"
--        y las conexiones del grafo dado son : [("a","b"),("c","a")]
--     El resultado sería: ["c","a","b"]

rutaMasCortaAux :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
rutaMasCortaAux from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)

--  Dada una lista de elementos, retorna las combinaciones de 2 elementos
--  entre sus elementos.
--  Ejemplo: Dada a lista [A,B,C,D]
--      resultado: [[A,B],[A,C],[A,D],[B,C],[B,D],[C,D]]

combinaciones::Eq a =>[a] -> [[a]]
combinaciones [] = []
combinaciones x = combinaciones_N 2 (nub x) 

-- Es una segunda definición de combinaciones, más general
-- recibe un k que es la agrupación que se le va a dar a los elementos
-- y la lista que tiene los elementos.
-- retorna una lista con las combinaciones de n elementos, agrupados de k en k.

combinaciones_N :: Integer -> [a] -> [[a]]
combinaciones_N 0 _          = [[]]
combinaciones_N _ []         = []
combinaciones_N k (x:xs) = 
    [x:ys | ys <- combinaciones_N (k-1) xs] ++ combinaciones_N k xs  

-- Recibe las combinaciones posibles entre los nodos de un grafo,
-- y el grafo.
-- calcula la ruta más corta entre cada combinación, si el calculo
-- de alguna de estas combinaciones da 0, que significa que no hay conexion
-- entre dos nodos, agrega un 0 a la lista y termina el algoritmo.
-- mientras hayan conexiones entre los nodos ejecuta el algoritmo,
-- y lo agrega a la lista que retorna
-- la lista de retorno contiene los costos minimos
-- para trasladarse de un nodo a otro, para todos los nodos del grafo.

algoritmo :: Ord a => [[a]] -> Map a [(a, Float)] -> [Int]
algoritmo [] graph = []
algoritmo conexiones graph 
 | (rutaMasCorta ((head conexiones) !! 0) ((head conexiones) !! 1) graph) == 0 = [0]
 | otherwise = [rutaMasCorta ((head conexiones) !! 0) ((head conexiones) !! 1) graph] ++ algoritmo (tail conexiones) graph


-- Recibe una lista de enteros(costos minimos resultantes de la funcion algoritmo)
-- y si encuentra un 0, entonces dice que el grafo es inconexo
-- en otro caso busca el mayor de los costos mínimos, y ese es el resultado.

resultadoF :: Int-> [Int] -> String
resultadoF numRed minimos
 |0 `elem` minimos = "Red "++ show numRed++": Inconexa."
 | otherwise = "Red "++ show numRed++": " ++ show (maximum minimos)

-- Toma una lista y retorna una lista con dos sublistas,
-- que contiene los elementos en indices pares en una sublista
-- y los elementos en indices impares en otra lista
particionarListaAux:: Ord a =>[a] -> [[a]]
particionarListaAux [] = []
particionarListaAux lista = particionarLista 0 lista [[],[]]


particionarLista :: Ord a =>Int -> [a] -> [[a]]-> [[a]]
particionarLista _ [] lr = lr
particionarLista  k l1 lr 
   | mod k 2 == 0 = particionarLista (k+1) (tail l1) [([head l1] ++ lr!!0),lr!!1]
   | otherwise = particionarLista (k+1) (tail l1) [lr!!0,([head l1] ++ lr!!1)]

-- Toma dos listas y acomoda las conexiones en tuplas.
-- Ejemplo: dada la lista [[A,B,C],[D,E,F]]
--  resultado: [(A,D),(B,E),(C,F)]
acomodarConexiones ::  Ord a => [[a]]-> [(a,a)]
acomodarConexiones lista = zip (lista!!0) (lista!!1)

--Encapsula los metodos necesarios para dar solución
-- al problema asignado, retorna la lista de costos minimos de un grafo
principal :: Ord a => [a]-> [Int]
principal lconexiones = algoritmo (combinaciones lconexiones) (construirGrafo lconexiones)

-- Recibe el numero de grafo que se esta analizando
-- y la lista con conexiones, e indica si es o no conexo.
principalAux :: Ord a => Int-> [a]-> String
principalAux numGrafo lconexiones = resultadoF numGrafo (principal lconexiones)
import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

buildGraph :: Ord a => [(a, a)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b) -> [(a,[(b,1)]), (b,[(a,1)])]



dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing)) 
                | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e


shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> Int
shortestPath from to graph = length (shortestPathAux from to graph) - 1


shortestPathAux :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPathAux from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)

combinaciones::Eq a =>[a] -> [[a]]
combinaciones [] = []
combinaciones x = combinaciones_N 2 (nub x) 

-- 2ª definición
combinaciones_N :: Integer -> [a] -> [[a]]
combinaciones_N 0 _          = [[]]
combinaciones_N _ []         = []
combinaciones_N k (x:xs) = 
    [x:ys | ys <- combinaciones_N (k-1) xs] ++ combinaciones_N k xs  


algoritmo :: Ord a => [[a]] -> Map a [(a, Float)] -> [Int]
algoritmo [] graph = []
algoritmo conexiones graph 
 | (shortestPath ((head conexiones) !! 0) ((head conexiones) !! 1) graph) == 0 = [0]
 | otherwise = [shortestPath ((head conexiones) !! 0) ((head conexiones) !! 1) graph] ++ algoritmo (tail conexiones) graph

redesInterconexas :: [Int] -> String

redesInterconexas minimos
 |0 `elem` minimos = "Red inconexa"
 | otherwise = "Red conexa, minimo: " ++ myIntToStr (maximum minimos)

myIntToStr :: Int -> String
myIntToStr x = show x 

main :: IO ()
main = do let g = buildGraph [("a","c"), ("a","d"), ("b","a")
                             ,("b","d"), ("c","d"), ("c","e")
                             ,("d","e"),("f","g")]
          print $ shortestPathAux "b" "e" g 
          --print $ g
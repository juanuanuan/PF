import Data.List 
import Data.Char 

_any :: (a -> Bool) -> [a] -> Bool 
_any f [] = False 
_any f (h:t) | f h = True  
             | otherwise = _any f t 

_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
_zipWith f [] _ = []
_zipWith f _ [] = []
_zipWith f (h:t) (k:s) = (f h k) : _zipWith f t s    

_takeWhile :: (a -> Bool) -> [a] -> [a]
_takeWhile f [] = []
_takeWhile f (h:t) | f h = h : _takeWhile f t 
                   | otherwise = []


_dropWhile :: (a -> Bool) -> [a] -> [a]
_dropWhile f [] = []
_dropWhile f (h:t) | f h = _dropWhile f t 
                   | otherwise = (h:t)

_span :: (a -> Bool) -> [a] -> ([a],[a])
_span f [] = ([],[])
_span f (h:t) | f h = (h:take,drop)
              | otherwise = ([],h:t)
              where (take,drop) = _span f t 

_deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
_deleteBy f x [] = []
_deleteBy f x (h:t) | f x h = t 
                    | otherwise = h : _deleteBy f x t 

_sortOn :: Ord b => (a -> b) -> [a] -> [a]
_sortOn f [] = []
_sortOn f (h:t) = sortAux f h (_sortOn f t)  


sortAux :: Ord b => (a -> b) -> a -> [a] -> [a]  
sortAux f x [] = [x] 
sortAux f x (h:t) | f x < f h = x : h : t 
                  | otherwise = h : sortAux f x t                


type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio 
selgrau x (h:t) = filter (\(coef,expo) -> x == expo) (h:t) 

conta :: Int -> Polinomio -> Int 
conta _ [] = 0 
conta x poli = length (find (\(coef,expo) -> expo == x) poli)

grau :: Polinomio -> Int 
grau [] = 0 
grau poli = foldr (\(coef,expo) -> max expo) 0 poli

deriv :: Polinomio -> Polinomio 
deriv poli = map (\(coef,expo) -> if expo > 0 then (coef * fromIntegral(expo) , expo - 1) else (0,0)) poli 

calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0 
calcula x poli = sum (map(\(coef,expo) -> if expo > 0 then (coef * (x ^ expo)) else (coef)) poli) 

simp :: Polinomio -> Polinomio 
simp [] = []
simp poli = filter (\(coef,expo) ->  coef /= 0) poli 

mult :: Monomio -> Polinomio -> Polinomio  
mult (coef,expo) poli = map (\(coef1,expo1) -> (coef * coef1, expo + expo1) ) poli 

ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena poli = sortOn (\(coef,expo) -> expo ) poli -- porque nao posso usar a ordenaAux  
                           
        
ordenaAux :: Monomio -> Polinomio -> Polinomio 
ordenaAux (coef,expo) ((coef1,expo1):t) | expo > expo1 = (coef1,expo1) : ordenaAux (coef, expo) t
                                        | otherwise = ordenaAux (coef1,expo1) t 

normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza poli = map (\l -> foldr (\(coef,expo) (c,e)  -> (c + coef, max e expo))  (0,0) l ) (groupBy mesmoGrau (ordena poli))
                 where mesmoGrau (_,g1) (_,g2) = g1 == g2 

soma :: Polinomio -> Polinomio -> Polinomio 
soma [] [x] = [x]
soma polinomio1 polinomio2 = normaliza ((++) polinomio1 polinomio2) -- como a função normalliza recebe uma função l de grau superior, podemos escolher qual é essa função 

produto :: Polinomio -> Polinomio -> Polinomio 
produto [] [x] = [x]
produto poli1 poli2 = normaliza $ concatMap (\x -> mult x poli1) poli2

equiv :: Polinomio -> Polinomio -> Bool 
equiv [] _ = False 
equiv _ [] = False 
equiv poli1 poli2 = filter (\(coef,expo) -> (coef) /= 0) (ordena (normaliza poli1)) ==  filter (\(coef,expo) -> (coef) /= 0) (ordena (normaliza poli2))

type Mat a = [[a]]

dimOk :: Mat a -> Bool 
dimOK [x] = True 
dimOk (l:t) | length l == length (head t) = dimOK t 
            | otherwise = False  

dimMat :: Mat a -> (Int,Int)
dimMat (h:t) = foldl (\(n,m) linha -> (n+1,m)) (1,length h) t 

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat [x] _ = [x]
addMat m1 m2 = zipWith (zipWith (+)) m1 m2 

_transpose :: Mat a -> Mat a 
_trasnpose [x] = [x]
_transpose (h:k) = head [h] :  _transpose (k)  

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] _ = [] 
multMat m1 m2 = [[sum (zipWith (*) lin col) | col <- _transpose m2] | lin <- m1]

zipMat :: (a->b->c) -> Mat a -> Mat b -> Mat c 
zipMat f [] m = []
zipMat f m1 m2 = zipWith ( zipWith f) m1 m2 

tripSup :: Eq a => Num a => Mat a -> Bool 
tripSup (h:t) = all (\n -> n == 0) col && tripSup lin 
               where col = map head t
                     lin = map tail t 

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft (h:t) = map last (h:t) : rotateLeft (map init (h:t))
                     
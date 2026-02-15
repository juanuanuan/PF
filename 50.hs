
import Data.Char
import Text.ParserCombinators.ReadP (get)
import Control.Arrow (ArrowZero(zeroArrow))


_enumFromTo ::Int -> Int -> [Int]
_enumFromTo _ 0 = []
_enumFromTo n p = n : [n+p-1] 

_enumFromThenTo :: Int -> Int -> Int -> [Int]
_enumFromThenTo _ _ 0 = []
_enumFromThenTo n m p | n < p = n : _enumFromThenTo ((m + n - 1)) m p  

plusplus :: [a] -> [a] -> [a]
plusplus _ [] = []
plusplus [] _ = []
plusplus (h:y) l2 = h : plusplus y l2      

eclecl :: [a] -> Int -> a 
eclecl [n] 0 = n 
eclecl (h:t) n = eclecl t (n-1)

_reverse :: [a] -> [a] 
_reverse [] = []
_reverse (h:t) = last t : _reverse t ++ [h]

_take :: Int -> [a] -> [a] 
_take _ [] = []
_take n (h:t) = h : _take (n-1) t

_drop :: Int -> [a] -> [a]
_drop _ [] = []
_drop n (h:t) =  _drop (n-1) t 

_zip :: [a] -> [b] -> [(a,b)]
_zip [] [] = [] 
_zip (h:t) (k:s) = (h,k) : _zip t s 

_replicate :: Int -> a -> [a]
_replicate n s = s : _replicate (n-1) s 

_intersperce :: a -> [a] -> [a]
_intersperce _ [] = []
_intersperce n (h:t) = h : foldr (:) [n] t 

_group :: Eq a => [a] -> [[a]] 
_group [] = [] 
_group [h] = [[h]]
_group (h:t) | h == head t = (h:t) : _group t 
             | otherwise = [h] : _group t 

_concat :: [[a]] -> [a]
_concat [] = []
_concat [(h:t)] = h : _concat [t]

_inits :: [a] -> [[a]] 
_inits [] = []
_inits (h:t) = [] : _inits (init (h:t)) ++ [h:t]

_tails :: [a] -> [[a]]
_tails [] = []
_tails (h:t) = (h:t) : _tails t ++ [] -- ou [(h:t), _tails t]

_heads :: [[a]] -> [a]
_heads [] = []
_heads (h:t) = head h : _heads t 

_total :: [[a]] -> Int 
_total (h:t) = length h + _total t 

_fun :: [(a,b,c)] -> [(a,c)]
_fun [] = []
_fun ((x,y,z):t) = (x,z) : _fun t 

_cola :: [(String,b,c)] -> String 
_cola [] = []
_cola ((str,y,z):t) = str ++ _cola t 

_idade :: Int -> Int -> [(String,Int)] -> [String]
_idade _ _ [] = []
_idade n p ((str,x):t) | n - x >= p = str : _idade n p t 
                       | otherwise = _idade n p t 

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = [1]
powerEnumFrom n m | m >= 0 = 1 : powerEnumFrom n (n^(m-1))  
                  | otherwise = []                      

_isPrime :: Int -> Bool 
_isPrime n = isPrimeAux n 2 
              where isPrimeAux :: Int -> Int -> Bool 
                    isPrimeAux n m | n `mod` m == 0 = True 
                                   | m * n > n = False 
                                   | otherwise = isPrimeAux n (m + 1)  


_isPrefixOf :: Eq a => [a] -> [a] -> Bool
_isPrefixOf [] _ = False 
_isPrefixOf _ [] = False 
_isPrefixOf (h:t) (k:s) | h == k && head t == head s = True 
                        | (h:t) == (k:s) = True 
                        | otherwise = False  

_isSufixOf :: Eq a => [a] -> [a] -> Bool 
_isSufixOf l1 l2 | l1 == l2 = True 
                 | otherwise = _isSufixOf l1 (tail l2) 

_isSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
_isSubsequenceOf [] _ = False 
_isSubsequenceOf _ [] = False 
_isSubsequenceOf (h:t) (k:s) | h == k = _isSubsequenceOf t s  
                             | otherwise = _isSubsequenceOf (h:t) s 

_elemIndices :: Eq a => a -> [a] -> [Int]
_elemIndices _ [] = []
_elemIndices x (h:t) = _elemAux 0 x (h:t)    
                     where _elemAux :: Eq a => Int -> a -> [a] -> [Int]   
                           _elemAux k s (h:t) | s == h = k : _elemAux (k+1) s t -- k+1 significa que estamos a resgistar posições 
                                              | otherwise = _elemAux (k+1) s t 

_nub :: Eq a => [a] -> [a] 
_nub [] = []
_nub (h:t) | h == head t = _nub t 
           | otherwise = h : _nub t 

_delete :: Eq a => a -> [a] -> [a] 
_delete _ [] = [] 
_delete n (h:t) | h == n = t 
                | otherwise = h : _delete n t       

slashslash :: Eq a => [a] -> [a] -> [a]
slashslash [] _ = []
slashslash [x] [] = [x]
slashslash (h:t) (k:s) | h == k = slashslash s t 
                       | otherwise = h : slashslash (k:s) t   

_union :: Eq a => [a] -> [a] -> [a] 
_union [] _ = []
_union [x] [] = [x]
_union (h:t) (k:s) | h == k = h : _union (k:s) t 
                   | otherwise = h : k : _union s t  

_intersect :: Eq a => [a] -> [a] -> [a]
_intersect [] _ = []
_intersect [x] [] = [x] 
_intersect (h:t) (k:s) | k == h = h : k : _intersect s t 
                       | otherwise = _intersect (k:s) t 

_insert :: Ord a => a -> [a] -> [a]
_insert x [] = [x]
_insert x (h:t) | x < h = x : (h:t)
                | x == h = (h:t) 
                | otherwise = h : _insert x t 

_unwords :: [String] -> String 
_unwords [] = ""
_unwords (h:t) = h ++ " " ++ _unwords t 

_unlines :: [String] -> String 
_unlines [] = []
_unlines (h:t) = h ++ "\n" ++ _unwords t 

_pMaior :: Ord a => [a] -> Int 
_pMaior [] = 0 
_pMaior (h:t) = getIndex (maximum (h:t)) (h:t)  


getIndex :: Ord a => a -> [a] -> Int 
getIndex _ [] = -1 
getIndex x (h:t) | x == h = 0 
                 | otherwise = 1 + getIndex x t      


_lookup :: Eq a => a -> [(a,b)] -> Maybe b 
_lookup _ [] = Nothing 
_lookup x ((h,t):s) | x == h = Just t 
                    | otherwise = _lookup x s 


preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente (h:t) | h < head t = h : preCrescente t 
                   | otherwise = [h]

iSort :: Ord a => [a] -> [a]
iSort [] = [] 
iSort [h] = [h] 
iSort (h:t) = h : _insert h t 

menor :: String -> String -> Bool 
menor [] _ = True 
menor _ [] = False 
menor (h:t) (k:s) | ord h < ord k = True 
                  | ord h == ord k = menor t s 
                  | otherwise = False 

elemSet :: Eq a => a -> [(a,Int)] -> Bool 
elemSet _ [] = False 
elemSet x ((h,t):s) | x == h = True 
                    | otherwise = elemSet x s 

convertMSet :: [(a,Int)] -> [a] 
convertMSet [] = []
convertMSet ((h,t):s) | t == 0 = convertMSet s  
                      | otherwise = h : convertMSet ((h,t-1):s)


insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet x ((h,t):s) | x == h = [(h,t+1)]
                       | otherwise = insereMSet x s 

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x ((h,t):s) | x == h = (h,t-1) : s 
                       | x == h && t == 0 = s 

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = constroiMSetAux [] (h:t) 
                      where constroiMSetAux :: Ord a => [(a,Int)] -> [a] -> [(a,Int)] 
                            constroiMSetAux [] _ = []
                            constroiMSetAux l (h:t) = constroiMSetAux (insereMSet h l) t 
                                                        
_partitionEithers :: [Either a b] -> ([a],[b])
_partitionEithers [] = ([],[])
_partitionEithers (h:t) = 
                        let (esq,dir) = _partitionEithers t 
                        in case h of 
                            Left a -> (a:esq,dir)
                            Right b -> (esq,b:dir) 

_catMaybes :: [ Maybe a ] -> [a]
_catMaybes [] = []
_catMaybes  (Nothing:t) =  _catMaybes t 
_catMaybes (Just h : t) = h : _catMaybes t 

data Movimento = Norte | Sul | Este | Oeste 
               deriving Show 

caminho :: (Int,Int) -> (Int,Int) -> [Movimento] 
caminho (x1,y1) (x2,y2) | x1 == x2 && y1 == y2 = []
                        | x1 > x2 && y1 == y2 = Este : caminho (x1,y1) (x2-1,y2)
                        | x1 < x2 && y1 == y2 = Oeste : caminho (x1,y1) (x2+1,y2)
                        | x1 == x2 && y1 > y2 = Sul : caminho (x1,y1) (x2,y2-1)
                        | x1 == x2 && y1 < y2 = Norte : caminho (x1,y1) (x2,y2+1)
                       


hasloops :: (Int,Int) -> [Movimento] -> Bool
hasloops _ [] = False
hasloops (xc,yc) (Norte:t) = hasloopsAux (xc,yc) (xc,(yc+1)) t
hasloops (xc,yc) (Sul:t)   = hasloopsAux (xc,yc) (xc,(yc-1)) t
hasloops (xc,yc) (Este:t)  = hasloopsAux (xc,yc) ((xc+1),yc) t
hasloops (xc,yc) (Oeste:t) = hasloopsAux (xc,yc) ((xc-1),yc) t

hasloopsAux :: (Int,Int) -> (Int,Int) -> [Movimento] -> Bool
hasloopsAux _ _ [] = False
hasloopsAux x y _ | x == y = True
hasloopsAux i (xc, yc) (Norte:t) = hasloopsAux i (xc,(yc+1)) t
hasloopsAux i (xc, yc) (Sul:t)   = hasloopsAux i (xc,(yc-1)) t
hasloopsAux i (xc, yc) (Este:t)  = hasloopsAux i ((xc+1),yc) t
hasloopsAux i (xc, yc) (Oeste:t) = hasloopsAux i ((xc-1),yc) t


type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (a1,a2) (b1,b2)) : t) 
                                | (a1-b1) == (a2-b2) = 1 + contaQuadrados t 
                                | otherwise = 0 + contaQuadrados t


areaTotal :: [Rectangulo] -> Float                        
areaTotal [] = 0
areaTotal (h : t) = areaRet h + areaTotal t               

areaRet :: Rectangulo -> Float                            
areaRet (Rect (a1,a2) (b1,b2)) = abs((a1-b1)*(a2-b2))


data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int              
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (h:t) = 0 + naoReparar t






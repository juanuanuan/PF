import Data.Char 
import Data.List 

-- Ficha bastante focada na noção de recursividade.
-- Dada a origem bastante matemática de haskell, a recursividade é o "back bone" de qualquer função.
-- A recursividade ou definição recursiva, faz com que o compilador, ao ler a função já sabe o que tem de fazer até ao fim, sem termos de ser nós a introduzir todos os casos e todos os resultados possíveis.
-- A recursividade é a chamada da função às vareáveis em que queremos operar até ao fim d função.



dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2 * h : dobros t 

numOcorre :: Char -> String -> Int 
numOcorre _ "" = 0 
numOcorre char (h:t) | char == h = 1 + numOcorre char t 
                     | otherwise = numOcorre char t 

positivos :: [Int] -> Bool 
positivos (h:t) | h > 0 = positivos t 
                | otherwise = False 

soPos :: [Int] -> [Int] 
soPos [] = []
soPos (h:t) | h > 0 = h : soPos t 
            | otherwise = soPos t 

somaNeg :: [Int] -> Int 
somaNeg [] = 0 
somaNeg (h:t) | h < 0 = h + somaNeg t 
              | otherwise = somaNeg t 

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt (h:t) | length l > 3 = tresUlt t 
              | otherwise = l 
                 where l = (h:t)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 
nosPrimeiros _ [] = False 
nosPrimeiros n ((x,y):t) | n == x = True 
                         | otherwise = nosPrimeiros n t 

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+k,y+s,z+p) 
                        where (k,s,p) = sumTriplos t 


soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t 
                | otherwise = soDigitos t 
                   where isDigit :: Char -> Bool 
                         isDigit char | ord char >= 48 && ord char <= 57 = True 
                                      | otherwise = False 

minusculas :: [Char] -> Int 
minusculas [] = 0 
minusculas (h:t) | isLower h = 1 + minusculas t 
                 | otherwise = minusculas t 

nums :: String -> [Int]
nums "" = []
nums (h:t) = if isDigit h then digitToInt h : nums t else nums t 

type Polinomio = [Monomio]
type Monomio = (Float,Int)


conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta n ((coef,expo):t) | n == expo = 1 + conta n t 
                        | otherwise = conta n t 

grau :: Polinomio -> Int
grau [] = 0  
grau ((coef,expo):t) = grauAux 0 polinomio 
                      where polinomio = ((coef,expo):t)
                            grauAux :: Int -> Polinomio -> Int
                            graAux n [] = n  
                            grauAux n ((coef,expo):t) | n < expo = grauAux expo t 
                                                      | otherwise = grauAux n t  

selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((coef,expo):t) | n == expo && n > 0 = (coef,expo) : selgrau n t 
                          | otherwise = selgrau n t 

deriv :: Num Polinomio => Polinomio -> Polinomio 
deriv [] = []
deriv ((coef,expo):t) = (coef* fromIntegral(expo), expo-1) : deriv t    -- fromIntegral converte um int para float, para que se possa realizar operações entre floats

calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0 
calcula x ((coef,expo):t) = coef*(x^expo) + calcula x t 

simp :: Polinomio -> Polinomio 
simp [] = []
simp ((coef,expo):t) | coef == 0 = simp t 
                     | otherwise = (coef,expo) : simp t 
mult :: Monomio -> Polinomio -> Polinomio 
mult (0,0) [] = []
mult (coef,expo) ((coef1,expo1):t) = (coef*coef1, expo +expo1) : mult (coef,expo) t 

normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza polinomio = normalizaAux (0,0) polinomio 
                      where normalizaAux :: Monomio -> Polinomio -> Polinomio 
                            normalizaAux (coef1,expo1) ((coef2,expo2):t) | expo1 == expo2 = normalizaAux (coef1,expo1) t
                                                                         | otherwise = (coef2,expo2) : normalizaAux (coef1,expo1) t 

soma :: Polinomio -> Polinomio -> Polinomio 
soma polinomio [] = normaliza polinomio 
soma [] polinomio = normaliza polinomio 
soma ((coef1,expo1):s) ((coef2,expo2):t) = normaliza ((coef1+coef2,expo2) : soma s t )

produto :: Polinomio -> Polinomio -> Polinomio 
produto polinomio [] = polinomio 
produto [] polinomio = polinomio 
produto ((coef1,expo1):t) ((coef2,expo2):s) = (coef1 * coef2, expo1 + expo2) : produto t s 

ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena ((coef,expo):t) = ordenaAux (0,0) ((coef,expo):t)
                         where ordenaAux :: Monomio -> Polinomio -> Polinomio 
                               ordenaAux _ [] = []
                               ordenaAux (coef1,expo1) ((coef2,expo2):t) | expo1 < expo2 = (coef1,expo1) : ordenaAux (coef1,expo1) t
                                                                         | otherwise = ordenaAux (coef1,expo1) t

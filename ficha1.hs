import Data.List 
import Data.Char 
import GHC.Weak (Weak)

-- ##! Programação Funcional é um paradigma da programação bastante prático e striaghtforward. 
-- ##! É um paradigma que, grande parte ou até mesmo a totalidade das suas funções e programas são imutávies, i.e, ao longo do decorrer da função, o seu comportamanto não muda, apenas segue a sua sintaxe e lógica.  
-- ##! Para este pardigma, utilizamos a linguagem de programação Haskell. 
-- ##! Haskell é uma linguagem extremamente lógica, que segue única e exclusivamente, morfismo matemático.
-- ##! Ao contrário de linguagens como C ou Java, Haskell é bastante lazy, i.e, tentamos sempre escrever o mínimo possível na criação de funções.

perimetro :: Float -> Float 
perimetro r = 2 * pi * r 

dist :: (Double, Double) -> (Double,Double) -> Double 
dist (0,0) (0,0) = 0 
dist (x,y) (x1,y1) = sqrt ((x-x1)^2 + (y-y1)^2) -- sqrt é uma função pré-definida em haskell, que nos dá a raíz quadrada.

_primUlt :: [a] -> [a] 
_primUlt [] = []
_primUlt (h:t) =  last t : [h] -- A função last, dá nos apenas o último elemento da lista. 

multiplo :: Int -> Int -> Bool 
multiplo m n | m `mod` n == 0 = True -- mod significa o resto da divisão inteira. se for 0, significa que m é múltiplo de n.
             | otherwise = False 

truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar (h:t) | length l `mod` 2 == 0 = l -- neste caso o uso do mod é para verificar se o número de elementos da lista l é par.
                  | otherwise = t 
                     where l = (h:t) 

max2 :: Int -> Int -> Maybe Int 
max2 m n = if m > n then Just m else Nothing -- o Maybe é um resultado "binário". I.e, se cumprir com a função dá Just resultado, senão dá Nothing (não há variável que cumpra com a função).

max3 :: Int -> Int -> Int -> Maybe Int 
max3 m n p | m > n && m > p = Just m 
           | m < n && m > p = Just n 
           | p > m && p > n = Just p
           | otherwise = Nothing 

nRaizes :: Double -> Double -> Double -> Int 
nRaizes m n p | delta > 0 = 2 
              | delta == 0 = 1 
              | delta < 0 = 0 
                where delta = (n^2) - 4 * m * p -- o uso do where geralmente serve para definir variáveis que ainda não forma inicializadas. 


raizes :: Double -> Double -> Double -> [Double]
raizes m n p | nRaizes m n p == 0 = []
             | nRaizes m n p == 1 = [raiz]
             | nRaizes m n p == 2 = [raiz,raiz2]
                where raiz = (((-n)+ sqrt(n^2) - (4 * m * p))) / 2 * m 
                      raiz2 = (((-n) - sqrt(n^2) - (4 * m * p))) / 2 * m 

            
type Hora = (Int,Int) -- contrutor Hora. uma hora é definida por um par/tuplo de inteiros (horas, minutos).

valHora :: Hora -> Bool 
valHora (h,m) | h > 0 && h < 24 && m > 0 && m < 60 = True 
              | otherwise = False 

comparaHora :: Hora -> Hora -> Bool 
comparaHora (h,m) (h1,m1) | valHora (h,m) && valHora (h1,m1) && h1 > h = True -- !!ATENÇÃO!! os operadores boleanos "&&,||" só podem ser usados em funões que esperam um resultado boleano. 
                          | valHora (h,m) && valHora (h1,m1) && h1 == h && m1 > m = True 
                          | otherwise = False 

converteH :: Hora -> Int 
converteH (h,m) = (h * 60) + m 
 
converteM :: Int -> Hora 
converteM m = (div m 60, mod m 60) -- função div dá apenas o resultado de uma divisão, descartando o resto (Exemplo: 7 / 3 == 2).


difH :: Num Hora => Hora -> Hora -> Int 
difH h1 h2 = abs (converteH h1 - converteH h2) -- abs dá nos o valor absoluto (I.e, o módulo) (Exemplo: abs (-7) == 7).

addMtoH :: Hora -> Int -> Hora 
addMtoH (h,m) x = converteM (converteH (h,m) + x)

data Horas = H Int Int deriving (Show,Eq)

valHoras :: Horas -> Bool 
valHoras (H h m) | h > 0 && h < 24 && m > 0 && m < 60 = True 
                 | otherwise = False 

comparaHoras :: Horas -> Horas -> Bool 
comparaHoras (H h1 m1) (H h2 m2) | valHora (h1,m1) && valHora (h2,m2) && h2 > h1 = True
                                 | valHora (h1,m1) && valHora (h2,m2) && h2 == h1 && m2 > m1 = True 
                                 | otherwise = False    

converteHoras :: Horas -> Int 
converteHoras (H h m) = h*60 + m 

converteMinutos :: Int -> Horas 
converteMinutos m  = (H (div m 60) (mod m 60))

difHoras :: Horas -> Horas -> Int 
difHoras h1 h2 = abs (converteHoras h1 - converteHoras h2)

addMinHoras :: Int -> Horas -> Horas 
addMinHoras m (H h1 m1) = converteMinutos (converteHoras(H h1 m1) + m)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq) -- Explicação do contrutor: Um semáforo ou está verde, ou está amarelo, ou está vermelho. 
-- o "deriving (Show, Eq)", para já não tem significado em concreto, explico nas fichas que se procedem.
-- basicamente serve para dizer ao compilador, que o Semaforo deve aparecer no ecrã e tem a propriedade de igualdade (I.e os resultados são minimamante comparáveis).  


next :: Semaforo -> Semaforo 
next Verde = Amarelo -- se está verde passa para amarelo
next Amarelo = Vermelho -- amarelo passa para vermelho
next Vermelho = Verde  -- de vermelho passa outra vez para Verde. Desta forma, o programa sabe que após um cor, passa logo para a cor seguinte sem falhar nenhuma vez. 


stop :: Semaforo -> Bool 
stop semaforo | semaforo == Verde = True -- no verde o carro anda 
              | otherwise = False  -- no resto deve parar.

safe :: Semaforo -> Semaforo -> Bool 
safe semaforo1 semaforo2 | semaforo1 == Verde && semaforo2 == Vermelho = True 
                         | semaforo2 == Verde && semaforo2 == Vermelho = True 
                         | semaforo1 == Vermelho && semaforo2 == Vermelho = True 
                         | otherwise = False 

data Ponto = Cartesiano Double Double | Polar Double Double 
             deriving (Show, Eq)


posx :: Ponto -> Double 
posx (Cartesiano x y) = x 
posx (Polar dist angle) = dist * cos angle -- nestas funções são apenas noções matemáticas.

posy :: Ponto -> Double 
posy (Cartesiano x y) = y 
posy (Polar dist angle) = dist * sin angle 

raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar dist angle) = dist 

angulo :: Ponto -> Double 
angulo (Cartesiano x y) = atan y/x
angulo (Polar dist angle) = angle 

_dist :: Ponto -> Ponto ->  Double 
_dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
_dist (Polar dist1 angle1) (Polar dist2 angle2) = sqrt (dist1^2 + dist2^2 - 2*(dist1 * dist2 * cos(angle2 - angle1)))

_isLower :: Char -> Bool 
_isLower char | ord char >= 97 && ord char <= 122 = True -- consultar tabela de ASCii para ter noção dos valores núemrericos usados.
              | otherwise = False 


isDigit :: Char -> Bool 
isDigit char | ord char >= 48 && ord char <= 57 = True -- ord converte um char num integer.
             | otherwise = False 

isAlpha :: Char -> Bool 
isAlpha char | ord char >= 97 && ord char <= 122 && ord char >= 65 && ord char <= 90 = True 
             | otherwise = False 


toUpper :: Char -> Char 
toUpper char = chr (ord char - 32) --chr converte um integer num char.

intToDigit :: Int -> Char 
intToDigit x = chr (x - 9)





 
 
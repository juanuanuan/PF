import Data.List (sort, nub, group)

import Data.Char
import Control.Concurrent (yield)
data Hora = H Int Int 
           deriving (Show,Eq)            


type Etapa = (Hora,Hora)
type Viagem = [Etapa]

horaDepois_ :: Hora -> Hora -> Bool 
horaDepois_ (H h1 m1) (H h2 m2) | h1 > h2 = True 
                                | h1 == h2 && m1 > m2 = True 
                                | otherwise = False 

valido :: Etapa -> Bool 
valido (h1,h2) = horaDepois_ h1 h2 


viagemValida :: Viagem -> Bool 
viagemvalida [(h1,h2)] = valido (h1,h2)
viagemValida (e1:(h3,h4):t) | (valido e1) && (horaDepois_ h3 h2) = viagemValida ((h3,h4):t)
                            | otherwise = False 
                              where (h1,h2) = e1  

calculaViagem :: Viagem -> Etapa 
calculaViagem viagem = (h1, hn) 
                       where (h1,h2) = head viagem 
                             (h3,hn) = last viagem 

tempoEntreHoras :: Hora -> Hora -> Int 
tempoEntreHoras (H h1 m1 ) (H h2 m2) = abs (((h1*60)+m1) - ((h2*60)-m2))
                         
efectiveTime :: Viagem -> Int 
efectiveTime [] = 0
efectiveTime ((h1,h2):t) =   (tempoEntreHoras h1 h2) + efectiveTime t    


tempoEspera :: Viagem -> Int 
tempoEspera [(h1,h2)] = 0 
tempoEspera ((h1,h2):(h3,hn):t) = (tempoEntreHoras h2 h3) + tempoEspera ((h3, hn):t)



type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double | Polar Double Double 
             deriving (Show,Eq)


posx :: Ponto -> Double 
posx (Cartesiano x y) = x
posx (Polar dist angle) = dist * (cos angle)

posy :: Ponto -> Double 
posy (Cartesiano x y) = y
posy (Polar dist angle) = dist * (sin angle)


distOrigem :: Ponto -> Double 
distOrigem (Cartesiano x y) = sqrt (x^2 + y^2)
distOrigem (Polar dist angle) = dist

angleOrigem :: Ponto ->  Double 
angleOrigem (Cartesiano x y) | x > 0 =  atan (y/x)
                             | x < 0 && y >= 0 = (atan y/x) + pi
                             | x < y && y > 0 = atan (y/x) - pi
                             | x == 0 && y > 0 = pi / 2 
                             | x == 0 && y < 0 = -pi / 2 
angleOrigem (Polar dist angle ) = angle 


distFunc :: Ponto -> Ponto -> Double
distFunc (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
distFunc (Cartesiano x y) (Polar dist angle) = sqrt((posx (Polar dist angle) - x)^2 + (posy (Polar dist angle) - y)^2)
distFunc (Polar dist angle) (Cartesiano x y) = sqrt(( x - posx (Polar dist angle))^2 + (y - posy (Polar dist angle))^2)
distFunc (Polar dist1 angle1) (Polar dist2 angle2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
                                        where x2 = posx (Polar dist2 angle2)
                                              x1 = posx (Polar dist1 angle1)
                                              y2 = posy (Polar dist2 angle2)
                                              y1 = posy (Polar dist1 angle1)

lPoliSize :: Poligonal -> Double 
lPOliSize [p] = 0 
lPoliSize (p1:p2) = (distFunc p1 (head p2)) + lPoliSize p2  

boolzPoli :: Poligonal -> Bool 
boolazPoli [] = False 
boolzPoli [p] = True 
boolzPoli (p1:p2)  | posx p1 == posx (last p2) && posy p1 == posy (last p2) = True 
                    | otherwise = False 



data Figura = Circulo Ponto Double 
             | Retangulo Ponto Ponto 
             | Triangulo Ponto Ponto Ponto 
               deriving (Show, Eq)

triangula :: Poligonal -> [Figura]
triangula [] = []
triangula (p1:p2:pn) | length (p1:p2:pn) <= 3 = []
                     | otherwise = triangula2 p1 (p2:pn)
                       where triangula2 :: Ponto -> Poligonal -> [Figura]
                             triangula2 p [s] = []
                             triangula2 p (p1:p2:p3) = (Triangulo p p1 p2) : triangula2 p (p2:p3)
                           

area :: Figura -> Double 
area (Circulo p1 p2) = pi * p2
area (Triangulo p1 p2 p3) = 
    let a = distFunc p1 p2 
        b = distFunc p2 p3 
        c = distFunc p3 p1 
        s = (a+b+c) / 2
        in sqrt (s*(s-a)*(s-b)*(s-c))
area (Retangulo p1 p2) = abs ((posx p1 - posx p2) * (posy p1 - posy p2))

pLineArea :: Poligonal -> Double 
pLineArea p = pLineArea2 (triangula p)
            where pLineArea2 :: [Figura] -> Double 
                  pLineArea2 [] = 0
                  pLineArea2 (h:t) = (area h) + pLineArea2 t 

subtractPoints :: Ponto -> Ponto -> Ponto
subtractPoints p1 p2 = (Cartesiano ((posx p2)-(posx p1)) ((posy p2)-(posy p1)))

addPoints :: Ponto -> Ponto -> Ponto
addPoints p1 p2 = (Cartesiano ((posx p2)+(posx p1)) ((posy p2)+(posy p1)))


poliIdent :: Poligonal -> Ponto -> Poligonal 
poliIdent [] _ = []
poliIdent (p1:p2) p3 = (poliIdent2 (subtractPoints p1 p3)) (p1:p2)
                          where poliIdent2 :: Ponto -> Poligonal -> Poligonal 
                                poliIdent2 p [] = []
                                poliIdent2 p (p2:p3) = (addPoints p1 p2) : poliIdent2 p p3                 

vectorFactor :: Double -> Poligonal -> Poligonal
vectorFactor _ [x] = []
vectorFactor n (h:t) = (Cartesiano (n*x) (n*y)) : (vectorFactor n (t))
                        where (Cartesiano x y) = (subtractPoints h (head t)) 


data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String
               deriving Show 

type Nome = String 
type Agenda = [(Nome,[Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda 
acrescEmail ""  _ _ = []
acresEmail _ _ [] = []
acresEmail name email ((entrada, contacts):t) | name == entrada = (entrada,(contacts ++ [Email email])) :t 
                                              | otherwise = (entrada, contacts) : acrescEmail name email t 

returnEmail :: Nome -> Agenda -> Maybe [String] 
returnEmail nome ((entrada, contacts):t) | nome == entrada = Just (listEmails contacts)
                                         | otherwise = returnEmail nome t 
                                           where listEmails :: [Contacto] -> [String]  
                                                 listEmails [] = []
                                                 listEmails ((Email mail):t) = mail : listEmails t                                         

returnContacts :: [Contacto] -> [Integer]
returnContacts [] = []
returnContacts ((Casa num):t) = num : returnContacts t
returnContacts ((Trab num):t) = num :returnContacts t
returnContacts ((Tlm num):t) = num : returnContacts t 
returnContacts (h:t) = returnContacts t -- qualquer outra coisa, procurs no resto da lista 


returnCasa :: Nome -> Agenda -> Maybe Integer  
returnCasa _ [] = Nothing 
returnCasa nome ((entrada,contacts):t) | nome == entrada && temNumCasa contacts = Just (tlmCasa contacts)
                                       | otherwise = Nothing 
                                         where tlmCasa :: [Contacto] -> Integer -- funcao que dada uma lista de contactos apenas retorna o contacto de casa
                                               tlmCasa [] = 0
                                               tlmCasa (h:t) = tlmCasa t 
                                               temNumCasa :: [Contacto] -> Bool -- verifica se um individuo possui sequer contacto de casa
                                               temNumCasa [] = False 
                                               temNumCasa ((Casa num):t) = True


type Dia = Int 
type Mes = Int 
type Ano = Int 

data Data = D Dia Mes Ano 
            deriving Show 

type TabDN = [(Nome,Data)]

dataNascimento :: Nome -> TabDN -> Maybe Data
dataNascimento _ [] = Nothing 
dataNascimento nome ((entrada,dataz):t) | nome == entrada = Just dataz
                                        | otherwise = Nothing 
                                           
                                                  
calculaIdade :: Data -> Nome -> TabDN -> Maybe Int        
calculaIdade _ _ [] = Nothing 
calculaIdade (D dia mes ano) nome ((entrada,(D dia1 mes1 ano1)):t) | nome == entrada && ((mes1 > mes) || (dia1 > dia && mes1 == mes)) = Just (ano - ano1 - 1)  
                                                                   | nome == entrada = Just (ano - ano1)
                                                                   | otherwise = Nothing
                                                                   

anterior :: Data -> Data -> Bool
anterior (D d1 m1 y1) (D d2 m2 y2) | y1 > y2 = False
                                   | y1 == y2 && m1 > m2 = False
                                   | y1 == y2 && m1 == m2 && d1 >= d2 = False
                                   | otherwise = True


ordena :: TabDN -> TabDN
ordena [] = []
ordena ((key, D day month year):t) = ordenaAux ((key, D day month year):t) ((key, D day month year):t)


ordenaAux :: TabDN -> TabDN -> TabDN
ordenaAux lOrd [x] = lOrd
ordenaAux lOrd ((key, d1):(key2, d2):t) | anterior d1 d2 = ordenaAux lOrd ((key2, d2):t)
                                        | otherwise = ordenaAux list list
                                                where list = ([(key2, d2)] ++ (deleteDate (key2, d2) lOrd))

deleteDate :: (Nome,Data) -> TabDN -> TabDN
deleteDate _ [] = []
deleteDate (name, d)  ((name1, d1):t) | name == name1 = t
                                      | otherwise = (name1,d1) : deleteDate (name, d) t  


porIdade :: Data -> TabDN -> [(Nome, Int)]
porIdade d l = porIdadeAux d (reverse (ordena l))

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((name,D day month year): t) = (name, (idadeDate d (D day month year))) : porIdadeAux d t

idadeDate :: Data -> Data -> Int
idadeDate (D day month year) (D day1 month1 year1) | (month1 > month) || (day1 > day && month1 == month) = (year - year1 - 1) -- Ainda não fez
                                                   | otherwise = (year - year1 ) 


data Extracto = Ext Float [(Data, String, Movimento)]
            deriving Show

data Movimento = Credito Float | Debito Float
            deriving Show

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext init ((date, descr, Debito value):t)) min | value >= min = (Debito value) : extValor (Ext init t) min
                                                        | otherwise = extValor (Ext init t) min
extValor (Ext init ((date, descr, Credito value):t)) min | value >= min = (Credito value) : extValor (Ext init t) min
                                                         | otherwise = extValor (Ext init t) min  


filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext init ((date, descr, move):t)) descrList | elem descr descrList = (date, move) : filtro (Ext init t) descrList
                                                    | otherwise = filtro (Ext init t) descrList



creDeb :: Extracto -> (Float,Float)
creDeb ext = (sumCreds ext, sumDebs ext)

sumCreds :: Extracto -> Float 
sumCreds (Ext _ []) = 0
sumCreds (Ext init ((date, descr, Credito value):t)) = value + sumCreds (Ext init t)
sumCreds (Ext init (h:t)) = sumCreds (Ext init t)

sumDebs :: Extracto -> Float 
sumDebs (Ext _ []) = 0
sumDebs (Ext init ((date, descr, Debito value):t)) = value + sumDebs (Ext init t)
sumDebs (Ext init (h:t)) = sumDebs  (Ext init t)


saldo :: Extracto -> Float
saldo (Ext final []) = final
saldo (Ext init ((date, descr, Debito value):t)) = saldo (Ext (init - value) t)
saldo (Ext init ((date, descr, Credito value):t)) = saldo (Ext (init + value) t)
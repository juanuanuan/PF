import Data.List 
import Data.Char 

data BTree a = Empty 
             | Node a (BTree a) (BTree a)
             deriving Show 

exampleTree :: BTree Int
exampleTree = Node 6 
                 (Node 3 
                     (Node 2 Empty Empty) 
                     (Node 5 Empty Empty)) 
                 (Node 9
                     (Node 8 
                        (Node 7 Empty Empty) 
                        Empty) 
                     (Node 15 Empty Empty)) 



main :: IO()
main = print(aprovAv turmaExemplo) -- substituir exampleTree pelo nome da função 


altura :: BTree a -> Int 
altura Empty = 0 
altura (Node n l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int 
contaNodos Empty = 0 
contaNodos (Node n l r) = 1 + (contaNodos l) + (contaNodos r) 

folhas :: BTree a -> Int 
folhas Empty = 1 
folhas (Node n l r) = 0 + (folhas l) + (folhas r) 

prune :: Int -> BTree a -> BTree a 
prune _ Empty = Empty 
prune 0 tree = tree 
prune x (Node n l r) = Node n (prune (x-1) l) (prune (x-1) r)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] tree = [] 
path (h:t) (Node n l r) | h == True = n : path t r
                        | otherwise = n : path t l 


mirror :: BTree a -> BTree a 
mirror Empty = Empty 
mirror (Node n l r) = (Node n (mirror r) (mirror l))

zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f (Node n1 l1 r1) (Node n2 l2 r2) = (Node (f n1 n2)) (zipWithBT f l1 l2) (zipWithBT f r1 r2)

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (n1,n2,n3) l r) = ((Node n1 l1 r1), (Node n2 l2 r2), (Node n3 l3 r3))
                               where (l1,l2,l3) = unzipBT l 
                                     (r1,r2,r3) = unzipBT r    

minimo :: Ord a => BTree a -> a 
minimo (Node n Empty _) = n 
minimo (Node n l r) = minimo l 

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node n Empty r) = r 
semMinimo (Node n l r ) = (Node n (semMinimo l) r)  

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node n Empty r) = (n,r)
minSmin (Node n l r) = (n1, Node n l1 r) 
                     where (n1,l1) = minSmin l 

{--remove :: Ord a => a -> BTree a -> BTree a 
remove _ Empty = Empty 
remove x --}


type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)                           
                               
inscNum :: Numero -> Turma -> Bool 
inscNum _ Empty = False 
inscNum x (Node (num,_,_,_) l r ) | x == num = True 
                                  | x > num = inscNum x r 
                                  | otherwise = inscNum x l                           

inscNome :: Nome -> Turma -> Bool 
inscNome _ Empty = False 
inscNome x (Node (_,name,_,_) l r) | x == name = True 
                                   | otherwise = inscNome x l || inscNome x r 

trabEst :: Turma -> [(Numero,Nome)]
trabEst (Node (num,name,TE,clas) l r) = trabEst l ++ [(num,name)] ++ trabEst r
trabEst (Node (num,name,_,_) l r) = trabEst l ++ trabEst l  


nota :: Numero -> Turma -> Maybe Classificacao 
nota _ Empty = Nothing 
nota x (Node (num,_,_,classi) l r) | x == num = Just classi 
                                   | x > num = nota x r 
                                   | x < num = nota x l 
                                   | otherwise = Nothing 

percFaltas :: Turma -> Float 
percFaltas Empty = 0.0
percFaltas turma = contaFaltas turma / fromIntegral (contaNodos turma) -- o numero de faltas em percentagem é dado pelo numero de faltas a dividir pelo numero de alunos


contaFaltas :: Turma -> Float 
contaFaltas Empty = 0.0
contaFaltas (Node (_,_,_,Rep) l r) = 1 + contaFaltas l + contaFaltas r
contaFaltas (Node infoTurma l r) = contaFaltas l + contaFaltas r 


mediaAprov :: Turma -> Float 
mediaAprov Empty = 0.0
mediaAprov turma = fromIntegral (somaNotas turma )/ fromIntegral (alunosAprov turma ) --se usar função contaNodos, vou estar a contar com os alunos que reporvaram, e neste caso só posso contabilizar os alunos que passaram a cadeira 


somaNotas :: Turma -> Int
somaNotas Empty = 0
somaNotas (Node (_,_,_,Aprov grade) l r) = grade + somaNotas l + somaNotas r 

alunosAprov :: Turma -> Int 
alunosAprov Empty = 0 
alunosAprov (Node (_,_,_,Aprov _) l r) = 1 + alunosAprov l + alunosAprov r 
alunosAprov (Node infoClass l r) = alunosAprov l + alunosAprov r

aprovAv :: Turma -> Float 
aprovAv Empty = 0.0
aprovAv turma = fromIntegral (alunosAprov turma) / fromIntegral (contaNodos turma)




turmaExemplo :: Turma
turmaExemplo = Node (123, "Joao Silva", ORD, Aprov 16)
                (Node (100, "Maria Pereira", TE, Rep) 
                      Empty
                      (Node (110, "Carlos Santos", MEL, Aprov 12) Empty Empty))
                (Node (150, "Ana Costa", ORD, Faltou) 
                      (Node (140, "Luisa Ferreira", TE, Aprov 18) Empty Empty)
                      Empty)

tails_ :: [a] -> [[a]]
tails_ l = [drop n l | n <- [0..length l]]



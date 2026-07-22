import Data.List 
import Data.Char

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt 


calcula :: ExpInt -> Int 
calcula (Const n) = n 
calcula (Simetrico n) = (-1) * calcula n 
calcula (Mais n x) = calcula n + calcula x 
calcula (Menos n x) = calcula n - calcula x 
calcula (Mult n x) = calcula n * calcula x


infixa :: ExpInt -> String 
infixa (Const n) = "n"
infixa (Simetrico n) = "(-1) * " ++ "(" ++ infixa n ++ ")" 
infixa (Mais n x) = "(" ++ infixa n ++ infixa x ++ ")"
-- fazer para o resto


data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a 
soma (R a []) = a 
soma (R n t) = n + sum (map soma t)

altura :: RTree a -> Int 
altura (R n []) = 1 
altura (R n t) = 1 + maximum (map altura t)

prune :: Int -> RTree a -> RTree a 
prune _ (R n []) = R n []
prune x (R n t) = R n (map (prune (x-1) )t) 

mirror :: RTree a -> RTree a 
mirror (R n []) = R n []
mirror (R n t) = R n (reverse (map mirror t))

postorder :: RTree a -> [a]
postorder (R n []) = []
postorder (R n t) = (last (map postorder t)) ++ [n]

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

exemploLTree :: LTree Int
exemploLTree = (Fork
                 (Fork (Tip 1) (Tip 2)) 
                 (Fork (Tip 3) (Tip 4)) 
                )

ltSum :: Num a => LTree a -> a 
ltSum (Tip n) = n 
ltSum (Fork l r) = 0 + ltSum l + ltSum r -- so pus o 0 para se perceber que o primeiro nó, obviamente, não é uma folha 

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n] -- listamos as folhas apenas 
listaLT (Fork l r) = listaLT l ++ listaLT r 

ltHeight :: LTree a -> Int 
ltHeight (Tip n) = 1
ltHeight (Fork l r) =  1 + max (ltHeight l) (ltHeight r)  

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) 

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No a f1 f2) = (Node a l r, Fork l1 r1)
                        where (l,l1) = splitFTree f1 
                              (r,r1) = splitFTree f2 

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip n) = Just (Leaf n)  
joinTrees (Node n l1 r1) (Fork l2 r2) = Just (No n f1 f2)
                                        where Just f1 = joinTrees l1 l2
                                              Just f2 = joinTrees r1 r2 
joinTrees _ _ = Nothing 


joinTrees2 :: BTree a -> LTree b -> Maybe (FTree a b) -- segunda versao
joinTrees2 Empty (Tip n) = Just (Leaf n)
joinTrees2 (Node n l1 r1) (Fork l2 r2) =
    case (joinTrees2 l1 l2, joinTrees2 r1 r2) of 
         (Just f1, Just f2 ) -> Just (No n f1 f2)
         _ -> Nothing -- usando o case, a função é mais estável, apesar de ser mais dificil de compreender.
                      -- o case serve para apenas para verificar pattern matching dentro do "corpo" da função.
                      -- ou seja, o padrão que queremos analisar é o caso da recursividade nas subárvores da esquerda e da direita de cada árvore inicial.
                      -- se encontrar resultados na esquerda e na direita, dá nos Just (No n "resultado1" "resultado2"), recursivamente.
                      -- o "_" serve para dizer que qualquer coisa dá nothing. Aí vem o conceito de pattern matching que ja devem estar familiarizados, que no fundo,
                      -- diz nos que sabendo do caso em que, de facto, existem resultados vindos de uma procura numa árvore bem formada dá nos o resultado esperado, 
                      -- qualquer outra coisa (Exemplo: um unmatch de árvores (BTree com RTree), por exemplo, deve retornar nothing para a função nao crashar.).

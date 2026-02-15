import Data.List 
import Data.Char
import GHC.Arr (accum)


digitChar :: String -> (String,String)
digitChar "" = ("","")
digitChar list = (getDigit list, getChar list) 
                   where getDigit :: String -> String 
                         getDigit "" = ""
                         getDigit (h:t) | isDigit h = [h] ++ getDigit t 
                                        | otherwise = getDigit t 
                         getChar :: String -> String 
                         getChar "" = ""
                         getChar (h:t) | isAlpha h = [h] ++ getChar t 
                                       | otherwise = getChar t               


nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h == 0 = (x,y+1,z)
          | h > 0 = (x,y,z+1)
          | otherwise = (x+1,y,z)
              where (x,y,z) = nzp t 


_divMod :: Integral a => a -> a -> (a,a)
_divMod div div2 | div - div2 >= 0 = (accum + 1, accum2)
                 | otherwise = (0,div) 
                    where (accum, accum2) =  _divMod(div-div2) div2 


                    
                    
fromDigits :: [Int] -> Int 
fromDigits [] = 0 
fromDigits l = fromDigitsAux l 0 
               where fromDigitsAux :: [Int] -> Int -> Int 
                     fromDigitsAux [] _ = 0 
                     fromDigitsAux  (h:t) acc = fromDigitsAux t ((acc*10)+h)                      

_maxSumInit :: (Num a,Ord a) => [a] -> a 
_maxSumInit l = msiAux 0 0 l 
                where msiAux :: (Num a,Ord a) => a -> a -> [a] -> a 
                      msiAux max acc [] = 0
                      msiAux max acc (h:t) |  
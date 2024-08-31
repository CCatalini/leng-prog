module Lists (member, union, intersection, difference,
              insert, insertionSort, firsts,
              binaryToDecimal, toDecimal, toDec, decimal) where

import Data.Char (digitToInt, toUpper, intToDigit)


member:: Int -> [Int] -> Bool
member _ []      = False                    -- caso base = lista vacía
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys                        -- caso base = lista vacía + lista ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys           -- si x no está en ys, lo agrega a la lista ys con el operador ':'


intersection:: [Int] -> [Int] -> [Int]
intersection [] ys = []                      -- caso base = lista vacía
intersection (x:xs) ys
  | member x ys = x : intersection xs ys     -- si x está en ys, lo agrega a la lista de intersección
  | otherwise   = intersection xs ys         -- si x no está en ys, no lo agrega a la lista de intersección


-- Devuelve los elementos que están en la primera lista pero no en la segunda.
difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference (x:xs) ys
    | member x ys = difference xs ys
    | otherwise   = x : difference xs ys


insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys


insertionSort :: [Int] -> [Int]
insertionSort = foldr insert []


binaryToDecimal :: [Int] -> Int
binaryToDecimal = binaryToDecimalAux 0
  where
    binaryToDecimalAux acc [] = acc
    binaryToDecimalAux acc (x:xs) = binaryToDecimalAux (acc * 2 + x) xs

    
toDecimal :: Int -> [Int] -> Int
toDecimal base = toDecimalAux 0
  where
    toDecimalAux acc [] = acc
    toDecimalAux acc (x:xs) = toDecimalAux (acc * base + x) xs


toDec :: Int -> String -> Int
toDec base = toDecimal base . map (digitToInt . toUpper)


-- Same as `toDec` But use a list comprehension
decimal :: Int -> String -> Int
decimal base s = sum [digitToInt c * base ^ i | (c, i) <- zip (reverse s) [0..]]


firsts :: [a] -> [[a]]
firsts [] = []
firsts xs = [take n xs | n <- [1..length xs]]


--binaryAdd :: [Int] -> [Int] -> [Int]

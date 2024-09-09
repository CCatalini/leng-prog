module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)

type Frequency = (Int, Char)

frequencies :: String -> [Frequency]
frequencies str = insertionSort $ map swap $ Map.toList $ frequencyMap str


-- El constraint (Ord a) indica que el tipo a debe ser ordenable
-- Map a Int = [key=elem, value=frequency]
-- foldr recorre la lista de derecha a izquierda, y va acumulando el resultado en el segundo argumento de la función lambda
-- Map.insertWith (+) x 1 acc: si x no está en el map, lo inserta con valor 1, si ya está, le suma 1 al valor que ya tiene
frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty


-- inserta un elemento en una lista ordenada
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys


insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

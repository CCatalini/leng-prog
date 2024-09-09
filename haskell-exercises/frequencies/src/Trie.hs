module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit
  
data Trie a = Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

-- Bring definition from previous TP

left :: Trie a -> Trie a
left (Node l _) = l
left (Leaf _) = error "Left of: Leaf"


right :: Trie a -> Trie a
right (Node _ r) = r
right (Leaf _) = error "Right of: Leaf"


find :: Bits -> Trie a -> a
find [] (Leaf x) = x
find [] (Node _ _) = error "No more bits but still at a node"
find (F:bs) (Node l _) = find bs l
find (T:bs) (Node _ r) = find bs r
find _ (Leaf _) = error "No more bits to traverse"


decode :: [Bit] -> Trie a -> [a]
decode [] _ = []
decode bits trie =
    let (val, remainingBits) = decodeOne bits trie
    in val : decode remainingBits trie

decodeOne :: [Bit] -> Trie a -> (a, [Bit])
decodeOne [] (Leaf x) = (x, [])
decodeOne (F:bs) (Node left _) = decodeOne bs left
decodeOne (T:bs) (Node _ right) = decodeOne bs right
decodeOne bs (Leaf x) = (x, bs)
decodeOne [] (Node _ _) = error "No bits left but still at a node"


toList :: Trie a -> [(a, Bits)]
toList = toList' []

toList' :: Bits -> Trie a -> [(a, Bits)]
toList' prefix (Leaf x) = [(x, prefix)]
toList' prefix (Node l r) = toList' (prefix ++ [F]) l ++ toList' (prefix ++ [T]) r

module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit

-- Para definir un tipo de dato se pueden hacer varios constructores separados por `|`
-- En este caso, un Trie puede ser una hoja (Leaf) o un nodo con dos hijos que son otros Trie
data Trie a  = Leaf a
               | Node (Trie a) (Trie a)
               deriving (Eq, Show)

-- Simple Trie
-- t = (Leaf 'A' :-: Leaf 'B') :-: Leaf 'C'
--          t-Node                       (:-:)
--         /      \                     /     \
--     Node        C        =        (:-:)     C
--    /    \                        /     \
--   A      B                      A       B


-- devuelve EL HIJO izquierdo, por eso Trie a -> !Trie a!
-- t :: Trie Char
-- t = Node (Node (Leaf 'A') (Leaf 'B')) (Leaf 'C')
--         Node
--        /    \
--    Node      'C'
--    /   \
--  'A'   'B'
-- left t = Node (Leaf 'A') (Leaf 'B')  = toda la rama izq
-- right t = error porque es una hoja
left :: Trie a -> Trie a
left (Node l _) = l                             -- Si es un nodo, regresa el hijo izquierdo, y no importa que hay a derecha
left (Leaf _) = error "Left of: Leaf"           -- si es hoja, es un error ya que no tiene hijos


right :: Trie a -> Trie a
right (Node _ r) = r
right (Leaf _) = error "Right of: Leaf"
  

-- Busca un valor en el Trie usando la lista de Bits recibida
  -- F -> Izquierda
  -- T -> Derecha
find :: Bits -> Trie a -> a
find [] (Leaf x) = x                                    -- base -> no hay bits y estamos en una hoja -> regresa el valor de la hoja
find [] (Node _ _) = error "No more bits but still at a node"
find (F:bs) (Node l _) = find bs l                      -- si el bit es F, busca en el hijo izquierdo con el resto de bits y el nodo L
find (T:bs) (Node _ r) = find bs r                      -- si el bit es T, busca en el hijo derecho con el resto de bits y el nodo R
find _ (Leaf _) = error "No more bits to traverse"      -- error -> se llega a una hoja pero no hay mas bits



decode :: [Bit] -> Trie a -> [a]
decode [] _ = []                                        -- base -> no hay bits, lista vacia
decode bits trie =                                      -- recursivo
    let (val, remainingBits) = decodeOne bits trie      -- let (val, remainingBits) -> es la declaration de una variable
                                                       -- (que es una tupla) que es el resultado de decodeOne
    in val : decode remainingBits trie                  -- `in` especifica cómo construir el resultado final de la función decode
                                                       -- : (cons) construye una lista agregando val al frente de la lista resultante de la llamada recursiva a decode remainingBits trie


-- La función decodeOne toma una lista de bits y sigue el Trie basado en el primer bit de la lista:
  --Si el bit es F, se mueve hacia el hijo izquierdo del nodo y llama recursivamente a decodeOne con los bits restantes.
  --Si el bit es T, se mueve hacia el hijo derecho del nodo y llama recursivamente a decodeOne con los bits restantes.
  --Si no quedan más bits y se encuentra en una hoja (Leaf), devuelve el valor de esa hoja.
decodeOne :: [Bit] -> Trie a -> (a, [Bit])
decodeOne [] (Leaf x) = (x, [])
decodeOne (F:bs) (Node left _) = decodeOne bs left
decodeOne (T:bs) (Node _ right) = decodeOne bs right
decodeOne bs (Leaf x) = (x, bs)
decodeOne [] (Node _ _) = error "No bits left but still at a node"


toList :: Trie a -> [(a, Bits)]               -- [(a, Bits)] -> lista de tuplas donde el primer elemento es un valor y el segundo es una lista de bits
toList = toList' []

-- recorre el Trie y acumula los bits que llevan a cada hoja.
toList' :: Bits -> Trie a -> [(a, Bits)]
toList' prefix (Leaf x) = [(x, prefix)]       -- Hoja-> devuelve una tupla que contiene el valor de la hoja y el prefijo acumulado de bits.
toList' prefix (Node l r) = toList' (prefix ++ [F]) l ++ toList' (prefix ++ [T]) r
-- Nodo -> llama recursivamente a toList' para el subárbol izquierdo y derecho, agregando F al prefijo para el subárbol izquierdo y T para el derecho.
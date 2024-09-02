module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F
-- ord c:  Convierte el carácter c a su valor numérico en el código ASCII
-- testBit (ord c) (7-n): Verifica si el bit en la posición 7-n (en un total de 8 bits) del número correspondiente al carácter c está activado.
-- Si el bit está activado, devuelve T, de lo contrario, devuelve F.

charToBits :: Char -> Bits
charToBits c = [bitAt i c | i <- [0..7]]
-- [bitAt i c | i <- [0..7]]:
-- Genera una lista de 8 elementos, donde cada elemento es el bit en la posición i del carácter c.

bits::String -> Bits
bits = concatMap charToBits
-- concatMap = higher order function de haskell
-- map = aplica una función a cada elemento de una lista
-- concat = concatena una lista de listas



-- TODO: Repasar para final
type Solution = [Int]

queens::Int -> [Solution]
queens n = solve n
  where
    -- Genera todas las soluciones posibles para un tablero de tamaño n
    solve 0 = [[]]  -- Caso base: cuando no hay filas, la solución es una lista vacía.
    solve k = [q : qs | qs <- solve (k-1), q <- [1..n], safe q qs]

    -- Verifica que colocar una reina en la columna q no resulta en un ataque
    safe _ [] = True
    safe q qs = not (q `elem` qs || sameDiag q qs)

    -- Verifica si una reina en la columna q está en la misma diagonal que otras reinas
    sameDiag q qs = any (\(colDist, q') -> abs (q - q') == colDist) (zip [1..] qs)


-- Solution:
-- Una lista de números donde cada número indica la columna en la que se coloca una reina en cada fila.
-- Por ejemplo, [2, 4, 1, 3] para un tablero 4x4 significa:
   --Reina en la columna 2 de la fila 1.
   --Reina en la columna 4 de la fila 2.
   --Reina en la columna 1 de la fila 3.
   --Reina en la columna 3 de la fila 4.
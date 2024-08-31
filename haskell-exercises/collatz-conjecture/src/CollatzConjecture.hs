module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just (collatzSteps n 0)

collatzSteps :: Integer -> Integer -> Integer
collatzSteps 1 steps = steps    -- Si el número es 1, retorna el número de pasos
collatzSteps n steps
    | even n    = collatzSteps (n `div` 2) (steps + 1)  -- Si el número es par, divide entre 2 y aumenta el contador de pasos
    | otherwise = collatzSteps (3 * n + 1) (steps + 1)  -- Si el número es impar, multiplica por 3 y añade 1, aumentando el contador de pasos

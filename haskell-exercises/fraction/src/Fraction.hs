module Fraction (Fraction, add, sub, mul, divide, hcf, simplify) where

type Fraction = (Int, Int) -- (numerador, denominador)


add :: Fraction -> Fraction -> Fraction
add n d = simplify ((fst n * snd d) + (fst d * snd n), snd n * snd d)
-- a/b + c/d = (a*d + c*b) / b*d
-- n = (a, b) fst n = a y snd n = b
-- d = (c, d) fst d = c y snd d = d


sub :: Fraction -> Fraction -> Fraction
sub n d = simplify ((fst n * snd d) - (fst d * snd n), snd n * snd d)
-- a/b - c/d = (a*d - c*b) / b*d
-- n = (a, b) fst n = a y snd n = b
-- d = (c, d) fst d = c y snd d = d


mul :: Fraction -> Fraction -> Fraction
mul n d = simplify ((fst n * fst d), (snd n * snd d))
-- a/b * c/d = a*c / b*d
-- n = (a, b) fst n = a y snd n = b
-- d = (c, d) fst d = c y snd d = d


divide :: Fraction -> Fraction -> Fraction
divide n d = simplify (fst n * snd d, snd n * fst d)
-- (snd d, fst d) = (c, d) -> (d, c)


hcf :: Int -> Int -> Int
hcf n 0 = abs n
hcf n d = hcf d (n `mod` d)


simplify :: Fraction -> Fraction
simplify (a, b) = (a `div` divisor, b `div` divisor)
  where divisor = hcf a b

    
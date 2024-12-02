module Problems.Day02 (solution) where

import Text.Parsec (sepEndBy1, newline, char)

import Common.Parse (AocInput, aocParse, integer)
import Common.Helper (pairwise)
import Common.Solution (Day)

input :: AocInput () [[Integer]]
input = sepEndBy1 (sepEndBy1 integer (char ' ')) newline

without :: Integer -> [Integer] -> [Integer]
without i x = [q | (p, q) <- zip [0..] x, p /= i]

withoutPermutations :: [Integer] -> [[Integer]]
withoutPermutations x = [without (toInteger i) x | i <- [0..length x]] 

isSafeBasic :: [Integer] -> Bool
isSafeBasic x = ((all (>= 1) y) || (all (<= -1) y)) && (and [(abs i) <= 3 | i <- y])
    where
        y = map (uncurry (-)) . pairwise $ x

isSafeComplex :: [Integer] -> Bool
isSafeComplex x = (isSafeBasic x) || (any isSafeBasic (withoutPermutations x))

solution :: Day
solution = (
        show . length . filter isSafeBasic . aocParse input (),
        show . length . filter isSafeComplex . aocParse input ()
    )

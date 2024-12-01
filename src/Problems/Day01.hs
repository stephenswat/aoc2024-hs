module Problems.Day01 (solution) where

import Text.Parsec (sepEndBy1, newline, space, many)
import Data.List (sort)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

input :: AocInput () [(Integer, Integer)]
input = sepEndBy1 line newline
    where
        line = do
            n1 <- integer
            _ <- many space
            n2 <- integer
            return (n1, n2)

solveA :: [(Integer, Integer)] -> Integer
solveA = sum . map (abs . uncurry (-)) . (uncurry zip) . both sort . unzip
    where
        both f (x, y) = (f x, f y)

solveB :: [(Integer, Integer)] -> Integer
solveB ls = sum [x * toInteger (length [y | y <- l2, x == y]) | x <- l1]
    where
        (l1, l2) = unzip ls

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )

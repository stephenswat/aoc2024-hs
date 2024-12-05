module Problems.Day05 (solution) where

import Text.Parsec (newline, sepEndBy1, sepBy1, char, string)
import Data.Map (Map, insertWith, empty, findWithDefault)

import Common.Solution (Day)
import Common.Parse (AocInput, integer, aocParse)

middle :: [a] -> a
middle xs
    | length xs `mod` 2 == 0 = error "Even length"
    | otherwise = xs !! (length xs `div` 2)

input :: AocInput () (Map Integer [Integer], [[Integer]])
input = do
    d <- sepEndBy1 dep newline
    _ <- newline
    l <- sepEndBy1 (sepBy1 integer (char ',')) newline
    return (foldl (\o (a, b) -> insertWith (++) a [b] o) empty d, l)
    where
        dep = do
            n1 <- integer
            _ <- string "|"
            n2 <- integer
            return (n1, n2)

isValidBetween :: Map Integer [Integer] -> [Integer] -> [Integer] -> Integer -> Bool
isValidBetween m a b i = all (\x -> elem x b || not (elem x (a ++ b))) (findWithDefault [] i m)

isValid :: Map Integer [Integer] -> [Integer] -> Bool
isValid m v = isValidHelper [] v
    where
        isValidHelper _ [] = True
        isValidHelper xs (y:ys) = isValidBetween m xs ys y && isValidHelper (y:xs) ys

repair :: Map Integer [Integer] -> [Integer] -> [Integer]
repair m s = repairHelp [] s
    where
        repairHelp _ [] = []
        repairHelp ys xs
            | length c == 1 = (head c):(repairHelp (((head c):ys)) [x | x <- xs, x /= head c])
            | otherwise = error "Zero or multiple candidates"
            where
                c = filter (isValidBetween m xs ys) xs

solveA :: Map Integer [Integer] -> [[Integer]] -> Integer
solveA m s = sum . map middle . filter (isValid m) $ s

solveB :: Map Integer [Integer] -> [[Integer]] -> Integer
solveB m s = sum . map middle . map (repair m) . filter (not . isValid m) $ s

solution :: Day
solution = (
        show . uncurry solveA . aocParse input (),
        show . uncurry solveB . aocParse input ()
    )

module Problems.Day13 (solution) where

import Data.Maybe (catMaybes)
import Text.Parsec (sepBy1, string, newline)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

type Matrix = (Integer, Integer, Integer, Integer, Integer, Integer)

input :: AocInput () [Matrix]
input = sepBy1 mat newline
    where
        mat = do
            _ <- string "Button A: X+"
            i1 <- integer
            _ <- string ", Y+"
            i2 <- integer
            _ <- newline
            _ <- string "Button B: X+"
            i3 <- integer
            _ <- string ", Y+"
            i4 <- integer
            _ <- newline
            _ <- string "Prize: X="
            i5 <- integer
            _ <- string ", Y="
            i6 <- integer
            _ <- string "\n"
            return (i1, i2, i3, i4, i5, i6)

solveLinEq :: Matrix -> Maybe (Integer, Integer)
solveLinEq (aax, aay, abx, aby, yx, yy)
    | xm == 0 && ym == 0 = Just (x, y)
    | otherwise = Nothing
    where
        lcm1 = lcm aax aay
        abx1 = (lcm1 `div` aax) * abx
        aby1 = (lcm1 `div` aay) * aby
        yx1 = (lcm1 `div` aax) * yx
        yy1 = (lcm1 `div` aay) * yy
        aby2 = aby1 - abx1
        yy2 = yy1 - yx1
        lcm2 = lcm abx1 aby2
        aax3 = (lcm2 `div` abx1) * lcm1
        yx3 = (lcm2 `div` abx1) * yx1
        yy3 = (lcm2 `div` aby2) * yy2
        (x, xm) = (yx3 - yy3) `divMod` aax3
        (y, ym) = yy3 `divMod` lcm2

cost :: (Integer, Integer) -> Integer
cost (a, b)
    | a < 0 || b < 0 = 0
    | otherwise = 3 * a + b

matModB :: Matrix -> Matrix
matModB (aax, aay, abx, aby, yx, yy) = (aax, aay, abx, aby, yx + 10000000000000, yy + 10000000000000)

solution :: Day
solution = (
        show . sum . map cost . catMaybes . map solveLinEq . aocParse input (),
        show . sum . map cost . catMaybes . map (solveLinEq . matModB) . aocParse input ()
    )

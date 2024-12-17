module Problems.Day17 (solution) where

import Text.Parsec (sepBy1, string, char)
import Data.Bits (xor, testBit)
import Data.List (nub)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Lens.Operators ((&), (.~), (%~), (^.))

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

type Registers = (Integer, Integer, Integer)

input :: AocInput () (Registers, [Integer])
input = do
    _ <- string "Register A: "
    ra <- integer
    _ <- string "\nRegister B: "
    rb <- integer
    _ <- string "\nRegister C: "
    rc <- integer
    _ <- string "\n\nProgram: "
    is <- sepBy1 integer (char ',')
    return ((ra, rb, rc), is)

run :: Registers -> [Integer] -> Integer -> [Integer]
run rs is ip
    | ip >= ((toInteger . length $ is) - 1) = []
    | otherwise = case oc of
        0 -> run (rs & _1 %~ (\x -> x `div` (2 ^ ov))) is (ip + 2)
        1 -> run (rs & _2 %~ (`xor` op)) is (ip + 2)
        2 -> run (rs & _2 .~ (ov `mod` 8)) is (ip + 2)
        3 -> run rs is (if (rs ^. _1) == 0 then ip + 2 else op)
        4 -> run (rs & _2 %~ (`xor` (rs ^. _3))) is (ip + 2)
        5 -> let rv = (ov `mod` 8) in (rv:(run rs is (ip + 2)))
        6 -> run (rs & _2 .~ ((rs ^. _1) `div` (2 ^ ov))) is (ip + 2)
        7 -> run (rs & _3 .~ ((rs ^. _1) `div` (2 ^ ov))) is (ip + 2)
        _ -> error "Invalid opcode!"
    where
        oc = is !! (fromInteger ip)
        op = is !! (fromInteger (ip + 1))
        ov
            | op <= 3 = op
            | op == 4 = (rs ^. _1)
            | op == 5 = (rs ^. _2)
            | op == 6 = (rs ^. _3)
            | otherwise = error "Invalid operand"

solveA :: Registers -> [Integer] -> [Integer]
solveA rs is = run rs is 0

solveB :: Registers -> [Integer] -> Integer
solveB rs is = minimum [x | x <- possibilities is [] 0, (solveA (rs & _1 .~ x) is) == is]
    where
        possibilities [] _ _ = [0]
        possibilities (q:xs) r v =
            nub
            [ x + 8 * y
            | x <- [0..7]
            , let b = x `xor` 1
            , b /= 0 || (x == q `xor` 4)
            , and [testBit x (fromInteger ri) == v1 | (i, v1) <- r, let ri = i - v, ri >= 0, ri <= 2]
            , let nc = q `xor` b `xor` 4
            , let nrc = [(v + b + i, testBit nc (fromInteger i)) | i <- [0, 1, 2]]
            , and [i1 /= i2 || v1 == v2 | (i1, v1) <- r, (i2, v2) <- nrc]
            , let nr = nub (nrc ++ r)
            , y <- nub (possibilities xs nr (v + 3))
            ]

solution :: Day
solution = (
        show . uncurry solveA . aocParse input (),
        show . uncurry solveB . aocParse input ()
    )

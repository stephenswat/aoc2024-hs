module Problems.Day11 (solution) where

import Text.Parsec (sepBy1, char)
import Control.Monad (forM)
import Control.Monad.State.Lazy (State, evalState, get, modify)
import Data.Map (Map, empty, insert, lookup)

import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

input :: AocInput () [Integer]
input = sepBy1 integer (char ' ')

splitNum :: Integer -> (Integer, Integer)
splitNum i = (read lhs, read rhs)
    where
        si = show i
        (lhs, rhs) = splitAt ((length si) `div` 2) si

solve :: Integer -> [Integer] -> Integer
solve ng xs = evalState goAll empty
    where
        goAll = do
            rs <- forM xs (go ng)
            return (sum rs)
        go :: Integer -> Integer -> State (Map (Integer, Integer) Integer) Integer
        go 0 _ = return 1
        go n i = do
            m <- get
            case Data.Map.lookup (n, i) m of
                (Just q) -> return q
                (Nothing) -> do
                    v <- if i == 0 then do
                        go (n - 1) 1
                    else if ((length (show i)) `mod` 2) == 0 then do
                        let (lhs, rhs) = splitNum i
                        lv <- go (n - 1) lhs
                        rv <- go (n - 1) rhs
                        return (lv + rv)
                    else do
                        go (n - 1) (i * 2024)
                    modify (insert (n, i) v)
                    return v

solution :: Day
solution = (
        show . solve 25 . aocParse input (),
        show . solve 75 . aocParse input ()
    )

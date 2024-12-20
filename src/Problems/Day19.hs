module Problems.Day19 (solution) where

import qualified Data.ByteString.Char8 as S (ByteString, take, drop, length, pack, null)
import Data.Bifunctor (second)
import Data.Map (Map, empty, lookup, insert)
import Control.Monad.State.Lazy (State, evalState, get, modify)
import Text.Parsec (sepBy1, sepEndBy1, many1, letter, string, newline)

import Common.Parse (AocInput, aocParse)
import Common.Solution (Day)

input :: AocInput () ([S.ByteString], [S.ByteString])
input = do
    ps <- sepBy1 (many1 letter) (string ", ")
    _ <- string "\n\n"
    ws <- sepEndBy1 (many1 letter) newline
    return (map S.pack ps, map S.pack ws)

isPossible :: S.ByteString -> State (([S.ByteString], Map S.ByteString Integer)) Integer
isPossible w
    | S.null w = return 1
    | otherwise = do
        (ps, mp) <- get
        case Data.Map.lookup w mp of
            (Just x) -> return x
            (Nothing) -> do
                rs <- sequence [isPossible wr | p <- ps, let lp = S.length p, (S.take lp w) == p, let wr = S.drop lp w]
                modify (second (insert w (sum rs)))
                return (sum rs)

solveA :: [S.ByteString] -> [S.ByteString] -> Integer
solveA ps ws = toInteger . length . filter (\w -> (evalState (isPossible w) (ps, empty)) > 0) $ ws

solveB :: [S.ByteString] -> [S.ByteString] -> Integer
solveB ps ws = sum . map (\w -> evalState (isPossible w) (ps, empty)) $ ws

solution :: Day
solution = (
        show . uncurry solveA . aocParse input (),
        show . uncurry solveB . aocParse input ()
    )

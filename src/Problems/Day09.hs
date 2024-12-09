module Problems.Day09 (solution) where

import Data.List (uncons)
import Data.Sequence (Seq (..), index, fromList, (|>), (><), take, drop, singleton)
import Text.Parsec (digit, setState, getState, many)

import Common.Parse (AocInput, aocParse)
import Common.Solution (Day)

data Block
    = Data Integer Integer
    | Gap Integer
    deriving (Eq, Show)

input :: AocInput (Bool, Integer) (Seq Block)
input = do
    ts <- many token
    return (fromList ts)
    where
        token = do
            (e, i) <- getState;
            n <- digit
            if e then do
                setState (False, i + 1)
                return (Data (read [n]) i)
            else do
                setState (True, i)
                return (Gap (read [n]))

checksum :: Seq Block -> Integer
checksum l = go l 0
    where
        go :: Seq Block -> Integer -> Integer
        go Empty _ = 0
        go ((Gap n):<|xs) j = go xs (j + n)
        go ((Data n i):<|xs) j = sum [(q * i) | q <- [j..(j + n - 1)]] + go xs (j + n)

compactA :: Seq Block -> Seq Block
compactA Empty = Empty
compactA (b@(Data _ _):<|xs) = b:<|(compactA xs)
compactA (b@(Gap n):<|(xs:|>ls)) = case ls of
        (Gap _) -> compactA (b:<|xs)
        (Data m i) -> if n == m then
                (Data m i):<|(compactA xs)
            else if m < n then
                (Data m i):<|(compactA ((Gap (n - m)):<|xs))
            else
                (Data n i):<|(compactA (xs |> (Data (m - n) i)))
compactA _ = error "Invalid call to `compactA`"

compactB :: Seq Block -> Seq Block
compactB Empty = Empty
compactB (xs:|>ls) = case ls of
    (Gap _) -> (compactB xs) |> ls
    b@(Data m _) -> case candidate b of
        (Just ((j, bs), _)) -> (compactB ((Data.Sequence.take j xs) >< bs >< (Data.Sequence.drop (j + 1) xs))) |> (Gap m)
        _ -> (compactB xs) |> ls
    where
        split (Data n i) (Gap m)
            | n > m = Empty
            | n == m = singleton (Data n i)
            | n < m = fromList [(Data n i), (Gap (m - n))]
        split _ _ = Empty
        candidate b = uncons . filter (not . null . snd) $ [(k, split b (index xs k)) | k <- [0..(length xs) - 1]]

solution :: Day
solution = (
        show . checksum . compactA . aocParse input (True, 0),
        show . checksum . compactB . aocParse input (True, 0)
    )

{-# LANGUAGE TupleSections #-}

module Problems.Day20 (solution) where

import Data.Map (lookup, toList)

import Common.Solution (Day)
import Common.Geometry (Grid2D, readGrid2DWith, neighbours4, manhattan)
import Common.Algorithm (dijkstra)

data Tile = Empty | Wall | Start | End deriving (Eq)

readTile :: Char -> Tile
readTile '.' = Empty
readTile '#' = Wall
readTile 'S' = Start
readTile 'E' = End
readTile _   = error "Invalid tile"

solve :: Integer -> Grid2D Tile -> Integer
solve n g = case dijkstra (\x -> Data.Map.lookup x g == (Just End)) nl (map (, 0) ips) of
    (Just (t, _)) -> go t
    (Nothing) -> 0
    where
        ips = [p | (p, t) <- toList g, t == Start]
        nl p = [(q, 1) | q <- neighbours4 p, Data.Map.lookup q g == (Just Empty) || Data.Map.lookup q g == (Just End)]
        go [] = 0
        go (x:xs) = (toInteger . length $ [y | (d, y) <- zip [1..] xs, let m = manhattan x y, (d - m + 2) > 100, m <= n]) + (go xs)

solution :: Day
solution = (
        show . solve 2 . readGrid2DWith readTile,
        show . solve 20 . readGrid2DWith readTile
    )

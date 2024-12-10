module Problems.Day10 (solution) where

import Data.Map (lookup, keys)
import Data.Set (fromList, size)

import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours4)
import Common.Solution (Day)

allTrails :: Grid2D Integer -> Point2D -> Integer -> [Point2D]
allTrails g p v
    | Data.Map.lookup p g == (Just v) && v == 9 = [p]
    | Data.Map.lookup p g == (Just v) = concat [allTrails g q (v + 1) | q <- neighbours4 p]
    | otherwise = []

solveA :: Grid2D Integer -> Int
solveA g = sum [size . fromList $ allTrails g p 0 | p <- keys g]

solveB :: Grid2D Integer -> Int
solveB g = sum [length $ allTrails g p 0 | p <- keys g]

solution :: Day
solution = (
        show . solveA . readGrid2DWith (\x -> read [x]),
        show . solveB . readGrid2DWith (\x -> read [x])
    )

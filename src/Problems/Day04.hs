module Problems.Day04 (solution) where

import Data.Map (keys, lookup)
import Data.Set (fromList)
import Data.Maybe (catMaybes)

import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours8)
import Common.Solution (Day)

matchesWord :: Grid2D Char -> Point2D -> Point2D -> [Char] -> Bool
matchesWord _ _ _ [] = True
matchesWord g p@(px, py) d@(dx, dy) (x:xs) = (Data.Map.lookup p g) == (Just x) && (matchesWord g (px + dx, py + dy) d xs)

makesXmas :: Grid2D Char -> Point2D -> Bool
makesXmas g p@(px, py) = (Data.Map.lookup p g) == (Just 'A') && diag1 && diag2
    where
        diag1 = (fromList . catMaybes $ [Data.Map.lookup (px - 1, py - 1) g, Data.Map.lookup (px + 1, py + 1) g]) == (fromList "MS")
        diag2 = (fromList . catMaybes $ [Data.Map.lookup (px + 1, py - 1) g, Data.Map.lookup (px - 1, py + 1) g]) == (fromList "MS")

solveA :: Grid2D Char -> Integer
solveA g = toInteger . length . filter (\(p, q) -> matchesWord g p q "XMAS") $ [(p, q) | p <- (keys g), q <- neighbours8 (0, 0)]

solveB :: Grid2D Char -> Integer
solveB g = toInteger . length . filter (makesXmas g) $ keys g

solution :: Day
solution = (
        show . solveA . (readGrid2DWith id),
        show . solveB . (readGrid2DWith id)
    )

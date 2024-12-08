module Problems.Day08 (solution) where

import Data.Maybe (isJust, fromJust)
import Data.Map (lookup, member, keys)
import Data.Set (Set, size, fromList)

import Common.Solution (Day)
import Common.Geometry (Point2D, Grid2D, readGrid2DWith)

readChar :: Char -> Maybe Char
readChar '.' = Nothing
readChar c   = Just c

antiNodes :: Bool -> Point2D -> Point2D -> [Point2D]
antiNodes b  (x1, y1) (x2, y2)
    | b = [p | i <- [1..500], p <- (go i) ++ (go (-i))]
    | otherwise = go 1
    where
        go i =
            [ (x2 + i * (x2 - x1), y2 + i * (y2 - y1))
            , (x1 + i * (x1 - x2), y1 + i * (y1 - y2))
            ]

getAllAntiNodes :: Bool -> Grid2D (Maybe Char) -> Set Point2D
getAllAntiNodes b g = fromList $ q
    where
        k = [i | i <- keys g, isJust . fromJust $ (Data.Map.lookup i g)]
        q = [p | i <- k, j <- k, i /= j, (Data.Map.lookup i g == Data.Map.lookup j g), p <- antiNodes b i j, member p g]

solution :: Day
solution = (
        show . size . getAllAntiNodes False . readGrid2DWith readChar,
        show . size . getAllAntiNodes True . readGrid2DWith readChar
    )

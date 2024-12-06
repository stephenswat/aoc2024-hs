module Problems.Day06 (solution) where

import Data.Maybe (fromJust, isNothing)
import Data.Map (lookup, member, insert, keys)
import Data.Set (Set, size, empty, insert, map, member, toList)

import Common.Geometry (Point2D, Grid2D, readGrid2DWith)
import Common.Solution (Day)

data Tile
    = Empty
    | Wall
    | Start
    deriving (Show, Eq)

readTile :: Char -> Tile
readTile '.' = Empty
readTile '#' = Wall
readTile '^' = Start
readTile _   = error "Invalid tile"

solve :: Grid2D Tile -> Maybe (Set Point2D)
solve g = fmap (Data.Set.map fst) $ go empty i (0, -1)
    where
        i = head [p | p <- keys g, Data.Map.lookup p g == (Just Start)]
        go t p@(px, py) d@(dx, dy)
            | Data.Set.member (p, d) t = Nothing
            | not (Data.Map.member p g) = Just t
            | Data.Map.lookup np g == (Just Wall) = go (Data.Set.insert (p, d) t) p (cd d) 
            | otherwise = go (Data.Set.insert (p, d) t) np d
            where
                np = (px + dx, py + dy)
                cd (ddx, ddy) = (-ddy, ddx)

solveB :: Grid2D Tile -> Integer
solveB g = toInteger . length . filter isNothing $ [solve (Data.Map.insert p Wall g) | p <- toList . fromJust . solve $ g, Data.Map.lookup p g == (Just Empty)]

solution :: Day
solution = (
        show . size . fromJust . solve . readGrid2DWith readTile,
        show . solveB . readGrid2DWith readTile
    )

module Problems.Day15 (solution) where

import Data.List.Split (splitOn)
import Data.List.Extra (replace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (lookup, keys, insert, fromList, toList)

import Common.Geometry (Grid2D, Point2D, readGrid2DWith)
import Common.Cardinal (Direction (..), translate)
import Common.Solution (Day)

data Tile = Wall | Empty | Box | BoxLeft | BoxRight | Agent deriving (Show, Eq)

readInput :: String -> (Grid2D Tile, [Direction])
readInput s = (readGrid2DWith readMap (ss !! 0), map readDir . replace "\n" "" $ (ss !! 1))
    where
        ss = splitOn "\n\n" s
        readMap '#' = Wall
        readMap '.' = Empty
        readMap 'O' = Box
        readMap '@' = Agent
        readMap _   = error "Invalid map tile"
        readDir '>' = East
        readDir '^' = North
        readDir '<' = West
        readDir 'v' = South
        readDir _   = error "Invalid direction"

applyMoveBlind :: Point2D -> Direction -> Grid2D Tile -> Grid2D Tile
applyMoveBlind p d g = case Data.Map.lookup p g of
    (Just v) -> insert np v (insert p Empty g)
    _ -> error "Invalid move operation"
    where
        np = translate d p

applyMove :: Point2D -> Direction -> Grid2D Tile -> Maybe (Grid2D Tile)
applyMove p d g = case Data.Map.lookup np g of
    Just Empty -> Just (applyMoveBlind p d g)
    Just Wall -> Nothing
    Just Box -> g & (applyMove np d) <&> (applyMoveBlind p d)
    Just BoxLeft -> if d == North || d == South then
            g & (applyMove np d) >>= (applyMove (translate East np) d) <&> (applyMoveBlind p d)
        else
            g & (applyMove np d) <&> (applyMoveBlind p d)
    Just BoxRight -> if d == North || d == South then
            g & (applyMove np d) >>= (applyMove (translate West np) d) <&> (applyMoveBlind p d)
        else
            g & (applyMove np d) <&> (applyMoveBlind p d)
    _ -> error "Invalid move operation"
    where
        np = translate d p

score :: Grid2D Tile -> Integer
score g = sum [100 * y + x | p@(x, y) <- keys g, Data.Map.lookup p g == (Just Box) || Data.Map.lookup p g == (Just BoxLeft)]

solve :: Grid2D Tile -> [Direction] -> Grid2D Tile
solve g [] = g
solve g (d:ds) = case applyMove ap d g of
    (Just g') -> solve g' ds
    (Nothing) -> solve g ds
    where
        ap = head [p | p <- keys g, Data.Map.lookup p g == (Just Agent)]

transformB :: (Grid2D Tile, [Direction]) -> (Grid2D Tile, [Direction])
transformB (g, d) = (ng, d)
    where
        ng = fromList [((2 * x + dx, y), nt) | ((x, y), t) <- toList g, (dx, nt) <- zip [0..] (tileMap t)]
        tileMap Agent = [Agent, Empty]
        tileMap Box = [BoxLeft, BoxRight]
        tileMap i = [i, i]

solution :: Day
solution = (
        show . score . uncurry solve . readInput,
        show . score . uncurry solve . transformB . readInput
    )

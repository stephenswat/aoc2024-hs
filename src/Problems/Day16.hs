module Problems.Day16 (solution) where

import Data.Map (Map, empty, insert, lookup, keys)
import Data.Set (Set, insert, unions, size, singleton)
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy (State, evalState, get, modify)

import Common.Cardinal (Direction (..), Rotation (..), translate, rotate)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith)
import Common.Solution (Day)
import Common.Algorithm (dijkstra)

data Tile
    = Empty
    | Start
    | End
    | Wall
    deriving (Eq, Show)

readTile :: Char -> Tile
readTile '.' = Empty
readTile 'S' = Start
readTile 'E' = End
readTile '#' = Wall
readTile _   = error "Invalid tile!"

solve :: Grid2D Tile -> [(Integer, Set Point2D)]
solve g = evalState (go sp East 0 (singleton sp)) empty
    where
        sp = head [p | p <- keys g, Data.Map.lookup p g == (Just Start)]
        mc = solveCost g
        go :: Point2D -> Direction -> Integer -> Set Point2D -> State (Map (Point2D, Direction) Integer) [(Integer, Set Point2D)]
        go p d v w = do
            let k = (p, d)

            if Data.Map.lookup p g == (Just End) then do
                return [(v, Data.Set.insert p w)]
            else do
                m <- get
                let valid = case Data.Map.lookup k m of
                        (Just n) -> v <= n && v <= mc
                        (Nothing) -> True

                if valid then do
                    modify (Data.Map.insert k v)

                    let cf = let np = translate d p in if (not (Data.Map.lookup np g == (Just Wall))) then [go np d (v + 1) (Data.Set.insert p w)] else []
                    let cl = let nd = rotate RotateLeft d in [go p nd (v + 1000) w]
                    let cr = let nd = rotate RotateRight d in [go p nd (v + 1000) w]

                    cs <- sequence (concat [cf, cl, cr])
                    let css = concat cs

                    if null css then do
                        return []
                    else do
                        return [x | x <- css, fst x == (minimum . map fst $ css)]
                else do
                    return []

solveCost :: Grid2D Tile -> Integer
solveCost g = snd . fromJust . dijkstra (\(p, _) -> Data.Map.lookup p g == (Just End)) np $ [((sp, East), 0)]
    where
        sp = head [p | p <- keys g, Data.Map.lookup p g == (Just Start)]

        np (p, d) = concat [cf, cl, cr]
            where
                cf = if (not (Data.Map.lookup (translate d p) g == (Just Wall))) then [((translate d p, d), 1)] else []
                cl = [((p, rotate RotateLeft d), 1000)]
                cr = [((p, rotate RotateRight d), 1000)]

solution :: Day
solution = (
        show . solveCost . readGrid2DWith readTile,
        show . size . unions . map snd . solve . readGrid2DWith readTile
    )

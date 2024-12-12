module Problems.Day12 (solution) where

import Data.List (sort)
import Data.Map (lookup, keys)
import Data.Set (Set, difference, fromList, toList, null, findMin, member, insert, empty, size, map)
import Control.Monad (forM_)
import Control.Monad.State.Lazy (State, execState, get, modify)

import Common.Solution (Day)
import Common.Geometry (Grid2D, Point2D, readGrid2DWith, neighbours4)

scoreA :: Grid2D Char -> Set Point2D -> Integer
scoreA g s = a * p
    where
        a = toInteger . size $ s
        p = toInteger . length $ [q | t <- toList s, q <- neighbours4 t, Data.Map.lookup q g /= Data.Map.lookup t g]

scoreB :: Grid2D Char -> Set Point2D -> Integer
scoreB g s = a * q
    where
        a = toInteger . size $ s

        eu = fromList [(x, y) | p@(x, y) <- toList s, let p' = (x, y - 1), Data.Map.lookup p' g /= Data.Map.lookup p g]
        el = fromList [(y, x) | p@(x, y) <- toList s, let p' = (x - 1, y), Data.Map.lookup p' g /= Data.Map.lookup p g]
        er = fromList [(y, x + 1) | p@(x, y) <- toList s, let p' = (x + 1, y), Data.Map.lookup p' g /= Data.Map.lookup p g]
        ed = fromList [(x, y + 1) | p@(x, y) <- toList s, let p' = (x, y + 1), Data.Map.lookup p' g /= Data.Map.lookup p g]

        compress xs' = go (sort xs') empty
            where
                go [] _ = []
                go (x:xs) s'
                    | or [Data.Set.member x' s' | x' <- [x-1, x, x+1]] = go xs (insert x s')
                    | otherwise = x:(go xs (insert x s'))

        countEdges t = toInteger . length $ [j | mi <- m, j <- compress . fmap (\(x, _) -> x) . filter (\(_, x) -> x == mi) . toList $ t]
            where
                m = toList . Data.Set.map (\(_, x) -> x) $ t

        q = (countEdges eu) + (countEdges ed) + (countEdges el) + (countEdges er)

floodFill :: Grid2D Char -> Point2D -> Set Point2D
floodFill g p = execState (go p) empty
    where
        go :: Point2D -> State (Set Point2D) ()
        go q = do
            v <- get
            if member q v then
                return ()
            else do
                modify (insert q)
                forM_ [q' | q' <- neighbours4 q, Data.Map.lookup q g == Data.Map.lookup q' g] go

partition :: Grid2D Char -> [Set Point2D]
partition g = go []
    where
        go :: [Set Point2D] -> [Set Point2D]
        go s
            | Data.Set.null rk = s
            | otherwise = go ((floodFill g (findMin rk)):s)
            where
                rk = foldl difference (fromList . keys $ g) s

solveA :: Grid2D Char -> Integer
solveA g = sum . fmap (scoreA g) . partition $ g

solveB :: Grid2D Char -> Integer
solveB g = sum . fmap (scoreB g) . partition $ g

solution :: Day
solution = (
        show . solveA . readGrid2DWith id,
        show . solveB . readGrid2DWith id
    )

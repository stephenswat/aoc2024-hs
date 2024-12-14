module Problems.Day14 (solution) where

import Text.Parsec (sepEndBy1, string, newline)
import Data.Set (fromList, member)

import Common.Parse (AocInput, aocParse, integer)
import Common.Geometry (Point2D)
import Common.Solution (Day)

input :: AocInput () [(Point2D, Point2D)]
input = sepEndBy1 drone newline
    where
        drone = do
            _ <- string "p="
            px <- integer
            _ <- string ","
            py <- integer
            _ <- string " v="
            vx <- integer
            _ <- string ","
            vy <- integer
            return ((px, py), (vx, vy))

step :: [(Point2D, Point2D)] -> [(Point2D, Point2D)]
step = map step1
    where
        step1 ((px, py), v@(vx, vy)) = (((px + vx) `mod` 101, (py + vy) `mod` 103), v)

hasChristmasTree :: [(Point2D, Point2D)] -> Bool
hasChristmasTree ds = any go [(x, y) | x <- [0..100], y <- [0..102]]
    where
        s = fromList [p | (p, _) <- ds]
        go :: Point2D -> Bool
        go (x, y) = and [member (x', y') s | x' <- [x..x+5], y' <- [y..y+5]]

solveA :: [(Point2D, Point2D)] -> Integer
solveA ds = toInteger ((length q1) * (length q2) * (length q3) * (length q4))
    where
        fs = (iterate step ds) !! 100
        q1 = [p | (p@(px, py), _) <- fs, px <= 49, py <= 50]
        q2 = [p | (p@(px, py), _) <- fs, px <= 49, py >= 52]
        q3 = [p | (p@(px, py), _) <- fs, px >= 51, py <= 50]
        q4 = [p | (p@(px, py), _) <- fs, px >= 51, py >= 52]

solveB :: [(Point2D, Point2D)] -> Integer
solveB ds = fst . head . filter (hasChristmasTree . snd) $ is
    where
        is = zip [0..] (iterate step ds)

solution :: Day
solution = (
        show . solveA . aocParse input (),
        show . solveB . aocParse input ()
    )

{-# LANGUAGE TupleSections #-}

module Problems.Day18 (solution) where

import Text.Parsec (sepEndBy1, string, newline)
import Data.Maybe (isNothing)
import Data.List (inits)
import Data.Set (fromList, notMember)

import Common.Geometry (Point2D, neighbours4)
import Common.Algorithm (dijkstra)
import Common.Parse (AocInput, aocParse, integer)
import Common.Solution (Day)

input :: AocInput () [Point2D]
input = sepEndBy1 p2 newline
    where
        p2 = do
            x <- integer
            _ <- string ","
            y <- integer
            return (x, y)

solve :: [Point2D] -> Maybe Integer
solve p = fmap snd $ dr
    where
        ps = fromList p
        fp p'@(x, y) = x >= 0 && x <= 70 && y >= 0 && y <= 70 && notMember p' ps
        dr = dijkstra (== (70, 70)) (map (, 1) . filter fp . neighbours4) [((0, 0), 0)]

solution :: Day
solution = (
        show . solve . take 1024 . aocParse input (),
        show . last . fst . head . filter (isNothing . snd) . map (\x -> (x, solve x)) . inits . aocParse input ()
    )

module Problems.Day03 (solution) where

import Data.Maybe (catMaybes)
import Text.Parsec (Parsec, getState, putState, string, try, digit, anyToken, many1, (<|>))

import Common.Parse (AocInput, aocParse, countBetween)
import Common.Solution (Day)

mul :: Parsec String a (Maybe (Integer, Integer))
mul = do
    _ <- string "mul("
    o1 <- countBetween 1 3 digit
    _ <- string ","
    o2 <- countBetween 1 3 digit
    _ <- string ")"
    return (Just (((read o1) :: Integer), ((read o2) :: Integer)))

notmul :: Parsec String a (Maybe (Integer, Integer))
notmul = do
    _ <- anyToken
    return Nothing

inputA :: AocInput () [(Integer, Integer)]
inputA = do
    r <- many1 ((try mul) <|> notmul)
    return (catMaybes r)

inputB :: AocInput Bool [(Integer, Integer)]
inputB = do
    r <- many1 single
    return (catMaybes r)
    where
        stateUpdateDo = do
            _ <- string "do()"
            putState True
            return Nothing
        stateUpdateDont = do
            _ <- string "don't()"
            putState False
            return Nothing
        parsers = ((try stateUpdateDo) <|> (try stateUpdateDont) <|> notmul)
        single = do
            b <- getState
            if b then ((try mul) <|> parsers) else parsers

solution :: Day
solution = (
        show . sum . map (uncurry (*)) . aocParse inputA (),
        show . sum . map (uncurry (*)) . aocParse inputB True
    )

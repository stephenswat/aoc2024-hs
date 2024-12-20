module Common.Parse (aocParse, integer, countBetween, AocInput) where

import Data.Maybe (isJust)
import Text.Parsec (Parsec, try, runParser, many1, digit, optionMaybe, char, count, choice)

type AocInput s a = Parsec String s a

aocParse :: Parsec String s a -> s -> String -> a
aocParse p s i = case (runParser p s "" i) of
    Left err -> error (show err)
    Right v  -> v

integer :: Parsec String a Integer
integer = do {
        s <- optionMaybe (char '-');
        q <- many1 digit;
        return ((if isJust s then (-1) else 1) * (read q))
    }

countBetween :: Int -> Int -> Parsec String a b -> Parsec String a [b]
countBetween a b p = choice [try (count x p) | x <- reverse [a..b]]

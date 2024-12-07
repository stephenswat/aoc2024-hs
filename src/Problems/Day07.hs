module Problems.Day07 (solution) where

import Text.Parsec (sepBy1, sepEndBy1, string, char, newline)

import Common.Solution (Day)
import Common.Parse (AocInput, aocParse, integer)

data Expr
    = Val Integer
    | Add Expr Expr
    | Mul Expr Expr
    | Concat Expr Expr

input :: AocInput () [(Integer, [Integer])]
input = sepEndBy1 line newline
    where
        line = do
            n <- integer
            _ <- string ": "
            m <- sepBy1 integer (char ' ')
            return (n, m)

allExprs :: Bool -> [Integer] -> [Expr]
allExprs _ [] = error "Empty list of numbers!"
allExprs _ (x:[]) = [Val x]
allExprs q (x:xs) = [Mul b a | a <- lhs, b <- rhs] ++
                    [Add b a | a <- lhs, b <- rhs] ++
                    (if q then [Concat b a | a <- lhs, b <- rhs] else [])
    where
        lhs = allExprs q [x]
        rhs = allExprs q xs

evalExpr :: Expr -> Integer
evalExpr (Val i) = i
evalExpr (Add a b) = (evalExpr a) + (evalExpr b)
evalExpr (Mul a b) = (evalExpr a) * (evalExpr b)
evalExpr (Concat a b) = read ((show (evalExpr a)) ++ (show (evalExpr b)))

solve :: Bool -> Integer -> [Integer] -> Integer
solve b g xs
    | any ((== g) . evalExpr) . (allExprs b) . reverse $ xs = g
    | otherwise = 0

solution :: Day
solution = (
        show . sum . map (uncurry (solve False)) . aocParse input (),
        show . sum . map (uncurry (solve True)) . aocParse input ()
    )

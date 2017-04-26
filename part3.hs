
import Prelude hiding (Num)
-- Jiaxu Li
-- Exercise 3
-- first expression
data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving (Show)

-- alternative abstract syntax
data Op = Add | Multiply | Negate

        deriving (Show)

data Exp = Num Int
         | Apply Op [Exp]
         deriving (Show)




-- (a) Represent the expression -(3+4)*7 in the alternative abstract syntax.
expression = Apply Multiply [Apply Negate [Apply Add [Num 3, Num 4]], Num 7]


-- (b) What are the advantages or disadvantages of either representation?

--The advantages of first expresentation are that it doesn't need to write many words and it is easy to understand what it does.
--The disadvantages of alternative abstract syntax is that it needs to call Op again and again. 

-- (c) Define a function translate :: Expr -> Exp that translates expressions given in the first abstract syntax into
-- equivalent expressions in the second abstract syntax.
translate :: Expr -> Exp
translate (N a) = Num a
translate (Plus a b) = Apply Add [translate a, translate b]
translate (Times a b) = Apply Multiply [translate a, translate b]
translate (Neg a) = Apply Negate [translate a]
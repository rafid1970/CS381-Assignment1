--CS381 HW1
--Xiaoyi Yang
--Due 4-27


--Exercise 1. Mini Logo

--a)

data Cmd = Pen Mode
	| Moveto Pos Pos
	| Def Name Pars Cmd
	| Call Name Vals
	| Seq Cmd Cmd
	deriving Show

data Mode = Up | Down
data Pos = Pos1 Number | Pos2 Name
data Pars = Single1 Name | Many1 [Name]
data Vals = Single2 Int | Many2 [Int]

type Number = Int
type Name = String

--function that make show variable
instance Show Pos where
		show (Pos1 x) = show x
		show (Pos2 y) = show y
instance Show Mode where
		show (Up) = "Up"
		show (Down) = "Down"
instance Show Pars where
		show (Single1 x) = show x
		show (Many1 y) = show y
instance Show Vals where
		show (Single2 x) = show x
		show (Many2 y) = show y

		
		
		
--b)
{-
con syntax:
def vector
vector ::= cmd
		 = Cmd Cmd
		 = Pen Up Cmd
		 = Pen Up Cmd Cmd
		 = Pen Up Moveto (x1, y1) Cmd
		 = Pen Up Moveto (X1, y1) Cmd Cmd
		 = Pen up Moveto (X1, y1) Pen down Cmd
		 = Pen up Moveto (x1, y1) Pen down Cmd Cmd
		 = Pen up Moveto (x1, y1) Pen down Moveto (x2, y2) Cmd
		 = Pen up Moveto (x1, y1) Pen down Moveto (x2, y2) Pen up

-}
--in abstract syntax

vector :: Cmd
vector = Def "vector" (Many1 ["x1", "y1", "x2", "y2"])
				(Seq (Pen Up)
				(Seq (Moveto(Pos2 "x1")(Pos2 "y1"))
				(Seq (Pen Down)
				((Moveto(Pos2 "x2")(Pos2 "y2"))
				 ) ) ) )
				
{-
Space for more works
-}				
				
--c)
{-
each stair end in (i, i) when steps i. for example steps 1 end in (1, 1), steps 3 end in (3, 3)
steps:: Int -> Cmd
steps 0 = []
step x = --fill out-- Seq (x-1)
-}
{-
Professor said we need start with (0, 0), that means old works must be changed
So new works, it is start with 0, 0. But just another way to x, x.
-}
steps :: Int -> Cmd
steps 1 = Seq (Pen Up) (Seq (Moveto (Pos1 0) (Pos1 0))(Seq (Pen Down)(Seq (Moveto (Pos1 0) (Pos1 1))(Moveto (Pos1 1)(Pos1 1)))))
steps x = Seq (steps (x-1))(Seq (Moveto (Pos1 (x-1)) (Pos1 x))(Moveto (Pos1 x) (Pos1 x)))
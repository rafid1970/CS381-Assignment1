
{-
Assignment 1 CS381 part2
Xiaoyi Yang
-}

--a)

data Circuit = C Gates Links
data Gates = G Int Gatefn Gates | Nog
data Gatefn = And | Or | Xor | Not
data Links = L Int Int Int Int Links | Nol

--b)
{-
The half adder:
1:xor;
2:and;
from 1.1 to 2.1;
from 1.2 to 2.2;
-}

halfadder :: Circuit
halfadder = C (G 1 Xor (G 2 And Nog)) (L 1 1 2 1 (L 1 2 2 2 Nol))
 
--c)
--test result "\"1:xor;\\n2:and;\\nfrom 1.1 to 2.1;\\nfrom 1.2 to 2.2;\\n\"" I dont know whats happened 

printer :: Circuit -> String
printer (C gate Nol) = printGate gate
printer (C gate link) = printGate gate ++";\n" ++ printLink link

printGate:: Gates -> String
printGate (Nog) = ""
printGate (G x gfn Nog)= show x ++ printGatefn gfn
printGate (G x gfn gate) = show x ++ printGatefn gfn ++ ";\n" ++ printGate gate

printGatefn ::Gatefn -> String
printGatefn And = ":and"
printGatefn Or = ":or"
printGatefn Xor = ":xor"
printGatefn Not = ":not"

printLink :: Links -> String
printLink Nol = ""
printLink (L x1 x2 x3 x4 link) = "from "++ show x1 ++ "." ++ show x2 ++ " to " ++ show x3 ++"."++ show x4 ++ ";\n" ++ printLink link

instance Show Circuit where
		show = printer
instance Show Gates where
		show = printGate
instance Show Gatefn where
		show = printGatefn
instance Show Links where
		show = printLink


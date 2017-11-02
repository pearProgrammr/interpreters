module TyTest where

import Terms


test01 = Lambda "x" (MathOp Add (Var "x") (IntConst 1))
test02 = Apl (Lambda "x" (MathOp Add (Var "x") (IntConst 1))) (IntConst 1)
test03 = Lambda "x" (MathOp Add (IntConst 1) (Var "x") )
test04 = Lambda "y" (Lambda "x" (MathOp Add (Var "y") (Var "x")))
test05 = Apl (Lambda "y" (Lambda "x" (MathOp Add (Var "y") (Var "x")))) (IntConst 1)
test06 = Lambda "x" (Var "x")
test07 = Lambda "y" (Lambda "x" (Apl (Var "x") (Var "y")))
test08 = Lambda "z" (Lambda "y" (Lambda "x" (Apl (Apl (Var "x") (Var "z")) (Var "y"))))
test09 = Lambda "x" (Equals (Var "x") (IntConst 8))
test10 = Lambda "y" (Lambda "x" (Equals (Var "y") (Var "x")))
test11 = Lambda "x" (Equals (MathOp Add (Var "x") (IntConst 1)) (BoolConst True))
test12 = Lambda "x" (Apl test13 test13)
test13 = MathOp Add (IntConst 1) (Var "x")
test14 = MathOp Add (Var "x") (Var "x")
test15 = (Var "x")
test16 = MathOp Add (IntConst 1) (BoolConst True)
test17 = Lambda "x" eq
eq = Equals (Var "x") (BoolConst True)
test18 = Lambda "x" (If eq (IntConst 1) (IntConst 2))
test19 = Let "x" (BoolConst True) (If (Var "x") (IntConst 1) (IntConst 2))

testList = [test01, test02, test03, test04, test05, test06, test07, test08, test09, test10, test11, test12, test13, test14, test15] 

runTest4 tst = run (infStEr [] tst) [] 0


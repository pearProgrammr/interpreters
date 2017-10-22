module TypeSys where

import Data.Char
import Terms

type Id = String

data Type = TVar TyVar
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Show)

data TyVar = TyVar Id Int
           deriving (Eq, Show)

-- pretty print
prettyType :: Type -> String
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType (TFun x y) = prettyType x ++ " -> " ++ prettyType y
prettyType (TVar (TyVar id num)) = varNumToType num

varNumToType :: Int -> String
varNumToType n
    | n > 26 = chr totalAscii : varNumToType (n `mod` 26)
    | otherwise = [chr totalAscii]
    where baseAscii = 97 -- 97 is ascii decimal for 'a'
          totalAscii = n + baseAscii

type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []

class Types t where
  applySubst :: Subst -> t -> t
  tv    :: t -> [TyVar]

instance Types Type where
  applySubst s (TVar v) = case lookup v s of
                          Just t -> t
                          Nothing -> TVar v
  applySubst s (TFun l r) = TFun (applySubst s l) (applySubst s r)
  applySubst s t          = t

  tv (TVar u) = [u]
  tv (TFun l r) = unique (tv l ++ tv r)
                  where unique (x:xs) = if x `elem` xs then xs else (x:xs)
                        unique []     = []
  tv _          = []

-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, applySubst s1 t) | (u, t) <- s2] ++ s1

--unification algorithm
unify TInt TInt = []
unify TBool TBool = []
unify (TVar v) (TVar w) = [reduceVarNum (TVar v) (TVar w) | v /= w]
                          where reduceVarNum (TVar (TyVar _ x)) (TVar (TyVar _ y))
                                     = if x < y
                                          then ((TyVar "none" x), (TVar (TyVar "none" y)))
                                          else ((TyVar "none" y), (TVar (TyVar "none" x)))
unify (TVar v) t = varBind v t
unify t (TVar v) = varBind v t
unify (TFun t1 t2) (TFun t1' t2')
  = let s1 = unify t1 t1'
        s2 = unify (applySubst s1 t2) (applySubst s1 t2')
      in (s2 @@ s1)
unify t1 t2 = error ("Cannot unify "++ show t1 ++ " with " ++ show t2)

-- if we have types that refer to each other in some circular fashion,
-- that's an error!!!
varBind v t = if (v `elem` vars t)
                 then error ("Occurs check fails!")
                 else [(v, t)]
              where vars (TVar v)   = [v]
                    vars TInt       = []
                    vars TBool      = []
                    vars (TFun l r) = vars l ++ vars r

type Env = [(Name, Type)]

extend env (n,t) = (n,t):env

envLookup :: Name -> Env -> Type
envLookup n env = case lookup n env of
                       Just t -> t
                       Nothing -> error ("unbound variable " ++ n)

applySubstToEnv :: Subst -> Env -> Env
applySubstToEnv s env = [(n, applySubst s t) | (n, t) <- env]

newVar n = (TVar (TyVar "none" n), n+1)

infer :: Env -> Term -> Int -> (Subst, Type, Int)

infer env (Var v) n
  = let t = envLookup v env
        in ([], t, n)

infer env (IntConst num) n
  = ([], TInt, n)

infer env (BoolConst b) n
  = ([], TBool, n)

infer env (Lambda v e) n
  = let (u, n1) = newVar n
        (s, t, n2) = infer ((v, u):env) e n1
    in (s, TFun (applySubst s u) t, n2)

infer env (Apl l r) n
  = let (s1, t1, n1) = infer env l n
        (s2, t2, n2) = infer (applySubstToEnv s1 env) r n1
        (v, n3) = newVar n2
        s3 = unify (applySubst s2 t1) (TFun t2 v)
    in (s3 @@ s2 @@ s1, applySubst s3 v, n3)

infer env (MathOp op num1 num2) n
  = let (s1, t1, n1) = infer env num1 n
        s1' = unify t1 TInt
        (s2, t2, n2) = infer (applySubstToEnv s1' env) num2 n1
        s2' = unify t2 TInt
    in (s2' @@ s1', TInt, n2)

-- note: this Let statement does not yet support a recursive let.
infer env (Let v x y) n
  = let (s1, t1, n1) = infer env x n
        (s2, t2, n2) = infer ((v, t1):(applySubstToEnv s1 env)) y n1
    -- We are only interested in the type of y. After this point, we know s2 and s1
    in (s2 @@ s1, t2, n2)

-- The type of the assignment is the type of the variable.
-- The type of the variable is the type of the term assigned to the variable.
infer env (Assign v t) n
  = infer env t n

-- we need to apply s2 to t1 to make sure that the t1 can be properly translated
-- to information we get from inferring y.
infer env (Equals x y) n
  = let (s1, t1, n1) = infer env x n
        (s2, t2, n2) = infer (applySubstToEnv s1 env) y n1
        s3 = unify (applySubst s2 t1) t2
    in (s3 @@ s2 @@ s1, TBool, n2)

infer env (If c e1 e2) n
  = let (s1, t1, n1) = infer env c n
        s1' = unify t1 (applySubst s1 TBool) -- the conditional needs to evaluate to a boolean!
        (s2, t2, n2) = infer (applySubstToEnv s1' env) e1 n1
        (s3, t3, n3) = infer (applySubstToEnv s2 env)  e2 n2
        s4 = unify (applySubst s3 t2) t3 -- we don't need s1'. s1' is a part of s2
    in (s4 @@ s3 @@ s2 @@ s1, t3, n3) -- do we need s1 here? -- the same variable in c can be used in e1 or e2


test1 = Lambda "x" (MathOp Add (Var "x") (IntConst 1))
test2 = Apl (Lambda "x" (MathOp Add (Var "x") (IntConst 1))) (IntConst 1)
test3 = Lambda "x" (MathOp Add (IntConst 1) (Var "x") )
test4 = Lambda "y" (Lambda "x" (MathOp Add (Var "y") (Var "x")))
test5 = Apl (Lambda "y" (Lambda "x" (MathOp Add (Var "y") (Var "x")))) (IntConst 1)
test6 = Lambda "x" (Var "x")
test7 = Lambda "y" (Lambda "x" (Apl (Var "x") (Var "y")))
test8 = Lambda "z" (Lambda "y" (Lambda "x" (Apl (Apl (Var "x") (Var "z")) (Var "y"))))
test9 = Lambda "x" (Equals (Var "x") (IntConst 8))
test10 = Lambda "y" (Lambda "x" (Equals (Var "y") (Var "x")))
test11 = Lambda "x" (Equals (MathOp Add (Var "x") (IntConst 1)) (BoolConst True))

getType (_,x,_) = x
runTest tst = prettyType  (getType  (infer [] tst 0))

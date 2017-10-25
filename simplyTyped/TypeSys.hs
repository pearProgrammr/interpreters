module TypeSys where

import Data.Char
import Terms


data Type = TVar TyVar
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Show)

data TyVar = TyVar Int
           deriving (Eq, Show)

-- pretty print
prettyType :: Type -> String
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType (TFun x y) = "(" ++ prettyType x ++ " -> " ++ prettyType y ++ ")"
prettyType (TVar (TyVar num)) = varNumToType num

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
                          where reduceVarNum (TVar (TyVar x)) (TVar (TyVar y))
                                     = if x < y
                                          then ((TyVar x), (TVar (TyVar y)))
                                          else ((TyVar y), (TVar (TyVar x)))
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
emptyEnv::Env
emptyEnv=[]

extend env (n,t) = (n,t):env

envLookup :: Name -> Env -> Type
envLookup n env = case lookup n env of
                       Just t -> t
                       Nothing -> error ("unbound variable " ++ n)

applySubstToEnv :: Subst -> Env -> Env
applySubstToEnv s env = [(n, applySubst s t) | (n, t) <- env]

-- Type inference-related code starts here
-- monadic style
data M a = M (Int -> (a, Int))
apply (M f) = f

instance Functor M where
  fmap f m = M (\n -> let (x, n') = apply m n
                      in  (f x, n'))

instance Applicative M where
  pure x  = M (\n -> (x, n))
  f <*> x = M (\n -> let (f1, n1) = apply f n
                         (x1, n2) = apply x n1
                     in (f1 $ x1, n2))

instance Monad M where
  -- (>>=) :: M a -> (a -> M b) -> M b
  x >>= f = M (\n -> let (x1, n1) = apply x n
                         M h = f x1
                     in h n1)

run :: M a -> Int -> a
run (M f) n = fst (f n)

nVar :: M Type
nVar = M (\n -> (TVar (TyVar n), n+1))

infer2 :: Env -> Term -> M (Subst, Type)
infer2 env (IntConst n)
  = do
    return ([], TInt)

infer2 env (BoolConst n)
  = do
    return ([], TBool)

infer2 env (Lambda v e)
  = do
    u <- nVar
    (s, t) <- infer2 ((v, u):env) e
    return (s, TFun (applySubst s u) t)

infer2 env (Var v)
  = do
    return ([], envLookup v env)

infer2 env (Apl l r)
  = do
    (s1, t1) <- infer2 env l
    (s2, t2) <- infer2 (applySubstToEnv s1 env) r
    v <- nVar
    return (unify (applySubst s2 t1) (TFun t2 v) @@ s2 @@ s1,
            applySubst (unify (applySubst s2 t1) (TFun t2 v)) v)

infer2 env (MathOp op num1 num2)
  = do
    (s1, t1) <- infer2 env num1
    (s2, t2) <- infer2 (applySubstToEnv (unify t1 TInt) env) num2
    return ((unify t2 TInt) @@ (unify t1 TInt), TInt)

infer2 env (Let v x y)
  = do
    (s1, t1) <- infer2 env x
    (s2, t2) <- infer2 ((v, t1):(applySubstToEnv s1 env)) y
    return (s2 @@ s1, t2)

infer2 env (Assign v t)
  = infer2 env t

infer2 env (Equals x y)
  = do
    (s1, t1) <- infer2 env x
    (s2, t2) <- infer2 (applySubstToEnv s1 env) y
    return (unify (applySubst s2 t1) t2 @@ s2 @@ s1, TBool)

infer2 env (If c e1 e2)
  = do
    (s1, t1) <- infer2 env c
    (s2, t2) <- infer2 (applySubstToEnv (unify t1 (applySubst s1 TBool)) env) e1
    (s3, t3) <- infer2 (applySubstToEnv s2 env)  e2
    return (unify (applySubst s3 t2) t3 @@ s3 @@ s2 @@ s1, t3)

-- monadic style (result)
data Res t = Err String | Answer t

instance Functor Res where
  fmap f (Answer t) = Answer (f t)
  fmap f (Err s) = Err s

instance Applicative Res where
  pure x = Answer x
  -- (<*>) :: Res (a -> b) -> Res a -> Res b
  f <*> x = case f of
              Answer f' -> fmap f' x
              Err s -> Err s

instance Monad Res where
  -- (>>=) :: Res a -> (a -> Res b) -> Res b
  x >>= f = case x of
              Answer x' -> f x'
              Err s -> Err s

getRes :: Res (Subst, Type, Int) -> Either (Subst, Type, Int) [Char]
getRes (Answer (x,y,z)) = Left (x,y,z)
getRes (Err s) = Right s


--unification algorithm
unify2 :: Type -> Type -> Res Subst
unify2 TInt TInt = return nullSubst
unify2 TBool TBool = return nullSubst
unify2 (TVar v) (TVar w) = return [(v, TVar w) | v /= w]
unify2 (TVar v) t = varBind2 v t
unify2 t (TVar v) = varBind2 v t
unify2 (TFun t1 t2) (TFun t1' t2')
  = do
    s1 <- unify2 t1 t1'
    s2 <- unify2 (applySubst s1 t2) (applySubst s1 t2')
    return (s2 @@ s1)

unify2 t1 t2 = Err ("Cannot unify "++ show t1 ++ " with " ++ show t2)

varBind2 :: TyVar -> Type -> Res Subst
varBind2 v t = if (v `elem` vars t)
                 then Err "Occurs check fails!"
                 else return [(v, t)]
              where vars (TVar v)   = [v]
                    vars TInt       = []
                    vars TBool      = []
                    vars (TFun l r) = vars l ++ vars r

infErr :: Env -> Term -> Int -> Res (Subst, Type, Int)
infErr env (Var v) n
  = return (nullSubst, envLookup v env, n)

infErr env (Lambda v e) n
  = do
    let (u, n1) = newVar n
    case infErr ((v, u):env) e n1 of
      Answer (s, t, n2) -> return (s, TFun (applySubst s u) t, n2)
      Err s -> Err s

infErr env (Apl l r) n -- gross
  = do
    case infErr env l n of
      Err s -> Err s
      Answer (s1, t1, n1) -> case infErr (applySubstToEnv s1 env) r n1 of
                               Err s' -> Err s'
                               Answer (s2, t2, n2) -> do
                                                      let (v, n3) = newVar n2
                                                      s3 <- unify2 (applySubst s2 t1) (TFun t2 v)
                                                      return (s3 @@ s2 @@ s1, applySubst s3 v, n3)
infErr env (IntConst num) n
  = return (nullSubst, TInt, n)

infErr env (MathOp op num1 num2) n
  = do
    case infErr env num1 n of
      Err s -> Err s
      Answer (s1, t1, n1) -> do
                             s1' <- unify2 t1 TInt
                             case infErr (applySubstToEnv (s1' @@ s1) env) num2 n1 of
                               Err s' -> Err s'
                               Answer (s2, t2, n2) -> do
                                                      s2' <- unify2 t2 TInt
                                                      return (s2' @@ s2 @@ s1' @@ s1, TInt, n2)


-- functional style
newVar n = (TVar (TyVar n), n+1)

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
        (s2, t2, n2) = infer (applySubstToEnv (s1' @@ s1) env) num2 n1
        s2' = unify t2 TInt
    in (s2' @@ s2 @@ s1' @@ s1, TInt, n2)

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

testList = [test01, test02, test03, test04, test05, test06, test07, test08, test09, test10, test11, test12, test13, test14, test15] 

getType (_,x,_) = x
runTest tst = prettyType (getType (infer emptyEnv tst 0))
runTest2 tst = prettyType (snd (run (infer2 emptyEnv tst) 0))

runTest3 tst = do
               printRes $ getRes $ infErr emptyEnv tst 0
               return ()

printRes (Left (a, b, c)) = do 
                            putStrLn (prettyType (getType (a, b, c)))
                            return ()

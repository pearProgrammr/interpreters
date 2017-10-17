module Types where


type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> State a -> State b
  fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))
  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s -> let (f,s')   = app stf s
                             (x, s'') = app stx s' in (f x, s''))
instance Monad ST where
  -- (>>=) :: a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

fresh :: ST Int
fresh = S (\n -> (n, n+1))

{-
{-# LANGUAGE MultiParamTypeClasses #-}
data EError a = EOk a | EFail String
instance Functor EError where
  -- fmap :: (a -> b) -> EError a -> EError b
  fmap f (EOk x) = EOk (f x)
  fmap f (EFail s) = EFail s

instance Applicative EError where
  -- pure :: a -> EError a
  pure = EOk
  --(<*>) :: EError (a -> b) -> EError a -> EError b
  EFail msg <*> EFail msg' = EFail (msg ++ msg')
  EFail msg <*> _ = EFail msg
  _ <*> EFail msg = EFail msg
  EOk f <*> EOk x = EOk (f x)

instance Monad EError where
  -- (>>=) :: EError a -> (a -> Error b) -> Error b
  EOk x >>= f = f x
  EFail msg >>= f = EFail msg
  
class Monad m => ErrorMonad m where
  mFail :: String -> m a

instance ErrorMonad EError where
  mFail = EFail

-- State monad definition
data State s a = ST (s -> (a,s))

instance Functor (State s) where
  fmap f (ST st) = ST (\s -> let (x, s') = st s in (f x, s'))

instance Applicative (State s) where
  pure x = ST (\s -> (x,s))
  -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  f <*> x = ST (\s -> let ST f1 = f
                          (f2, s1) = f1 s
                          ST x1 = x
                          (x2, s2) = x1 s1
                      in  (f2 x2, s2))
                          

instance Monad (State s) where
  -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  m >>= f = ST (\s -> let ST m' = m
                          (x, s1) = m' s
                          ST f' = f x
                          (y, s2) = f' s1
                      in  (y, s2))

class Monad m => StateMonad m s where
  update :: (s -> s) -> m s

instance StateMonad (State s) s where
  update f = ST (\s -> (s, f s))

incr :: StateMonad m Int => m Int
incr = update (1+)
-}

data Type v = TVar v
            | TInt
            | TBool
            | TFun (Type v) (Type v)

instance Functor Type where
  -- fmap :: (a -> b) -> Type a -> Type b
  fmap f (TVar v) = TVar (f v)
  fmap f (TInt)   = TInt
  fmap f (TBool)  = TBool
  fmap f (TFun v1 v2) = TFun (fmap f v1) (fmap f v2)

instance Applicative Type where
  -- pure :: a -> Type a
  pure x = TVar x
  -- <*> :: Type (a -> b) -> Type a -> Type b
  TInt  <*> _ = TInt
  TBool <*> _ = TBool
  _ <*> TInt = TInt
  _ <*> TBool = TBool
  TVar f <*> TVar x = TVar (f x)
  TVar f <*> TFun x y = TFun (fmap f x) (fmap f y)
--  TFun f g <*> TFun x y = TFun (fmap f x) (fmap g y) -- WHaaaa? I don't understand how this should work...

instance Monad Type where
  -- >>= :: Type a -> (a -> Type b) -> Type b
  TInt >>= _ = TInt
  TBool >>= _ = TBool
  TVar x >>= f = f x
  TFun x y >>= f = TFun (x >>= f) (y >>= f)

apply :: Monad m => (a -> m b) -> (m a -> m b)
apply s t = t >>= s --think of this as  "applying s to result "

-- this composes substitutions
(@@) :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g = join . fmap f . g -- map f to all values within g, collapse one monadic layer

-- this takes one monadic "level" off the input
join :: Monad m => m (m a) -> m a
join xss = xss >>= id

type Subst m v = v -> m v

-- Substitute v for t, leave everything else alone
(>>>) :: (Eq v, Monad m) => v -> m v -> Subst m v
(v >>> t) w = if v == w then t else return w

unify TInt TInt = return return
unify TBool TBool = return return
unify TInt TBool = fail ("can't unify TInt and TBool")
unify TBool TInt = fail ("can't unify TBool and TInt")

-- return the substitution. Note: retun is a null substitution
unify (TVar v) (TVar w) = return (if v == w then return 
                                            else (v >>> TVar w))
unify (TVar v) t = varBind v t
unify t (TVar v) = varBind v t
unify (TFun x y) (TFun u v) = do
                            s1 <- unify x u -- PASS THE SUBSTITUTION!
                            s2 <- unify y v
                            return (s1 @@ s2)

-- bind v to t check if they reference itself.
-- vars returns a list of variables
varBind v t = if (v `elem` vars t)
                then fail "Occurs check fails"
                else return (v>>>t)
              where vars (TVar v) = [v]
                    vars TInt = []
                    vars TBool = []
                    vars (TFun d r) = vars d ++ vars r

type Name = String
data Term = Var Name
          | Ap Term Term
          | Lam Name Term
          | Num Int

-- Env is a list of Assumptions about the types currently assigned to free variables
data Env t = Assump [(Name, t)]
emptyEnv :: Env t
emptyEnv = Assump []

extend :: Name -> t -> Env t -> Env t
extend v t (Assump assump) = Assump ((v,t):assump)


envlookup :: Name -> Env t -> t
envlookup v (Assump assump) = case lookup v assump of
                              Just t -> t
                              Nothing -> error ("Unbound variable: " ++ v)
--                         where find (w, t) alt = if w==v then t else alt
--                               err             = error ("Unbound variable: " ++ v)

instance Functor Env where
  fmap f (Assump as) = Assump [ (name, f t) | (name, t) <- as]



infer :: Env (Type Int) -> Term -> Int -> (Subst Type Int, Type Int, Int)

--type Subst m v = v -> m v

infer assumpts (Var v) st
  = do
    t <- envlookup v assumpts
    return (return, t, st)

--infer a (Lam v e)
--  = do
--    b <- fresh
--    (s,t) <- infer (extend v (TVar b) a) e
--    return (s, s b <$> t)
--
--
--infer a (Ap l r)
--  = do
--    (s, lt) <- infer a l
--    (t, rt) <- infer (fmap (apply s) a) r
--    b <- fresh
--    u <- unify (apply t lt) (rt <$> TVar b)
--    return (u @@ t @@ s, u b)


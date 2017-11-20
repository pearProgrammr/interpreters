module TyInfM where

import Type


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

data TyInf a = TyInf (CEnv -> Subst -> Int -> Res (a, CEnv, Subst, Int))

app (TyInf f) c s n = f c s n

instance Functor TyInf where
  fmap f x = TyInf (\c s n -> case app x c s n of
                            Err str -> Err str
                            Answer (x', c', s', n') -> Answer (f x', c', s', n'))

instance Applicative TyInf where
  pure x = TyInf (\c s n -> Answer (x, c, s, n))
  f <*> x = TyInf (\c s n -> case app f c s n of
                           Err str -> Err str
                           Answer (f', c', s', n') -> case app x c' s' n' of
                                                Err str -> Err str
                                                Answer (x', c'', s'', n'') -> Answer (f' x', c'', s'', n''))

instance Monad TyInf where
  -- f >>= g  :: TyInf a -> (a -> TyInf b) -> TyInf b
  x >>= f = TyInf (\c s n -> case app x c s n of
                             Err str -> Err str
                             Answer (x', c', s', n') -> let TyInf h = f x'
                                                       in h c' s' n')

run f c s n = printres (app f c s n)
               where printres (Err str) = do
                                          putStrLn str
                                          return ()
                     printres (Answer (t, c, s, n)) = do
                                          putStrLn (prettyType (apply s t))
                                          return ()

--unification algorithm
unite :: Type -> Type -> Res Subst
unite TInt TInt = return nullSubst
unite TBool TBool = return nullSubst
unite (TVar v) (TVar w) = return [(v, TVar w) | v /= w]
unite (TVar v) t = varBind v t
unite t (TVar v) = varBind v t
unite (TFun t1 t2) (TFun t1' t2')
  = do
    s1 <- unite t1 t1'
    s2 <- unite (apply s1 t2) (apply s1 t2')
    return (s2 @@ s1)
unite t1 t2 = Err ("Cannot unify "++ show t1 ++ " with " ++ show t2)

varBind :: TyVar -> Type -> Res Subst
varBind v t = if (v `elem` vars t)
                 then Err "Occurs check fails!"
                 else return [(v, t)]
              where vars (TVar v)   = [v]
                    vars TInt       = []
                    vars TBool      = []
                    vars (TFun l r) = vars l ++ vars r



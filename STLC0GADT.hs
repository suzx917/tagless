-- STLC with Bool, inital embedding using GADTs
-- from Oleg's tutorial

{-# LANGUAGE GADTs,
             DeriveFunctor,
             StandaloneDeriving
#-}

module STLC0GADT where

import Data.Map as M

data Var env t where
  V0 :: Var (t, env) t
  VS :: Var env      t -> Var (a, env) t
deriving instance Show (Var env t)

data Exp env t where
  B :: Bool                  -> Exp env Bool
  V :: Var env      t        -> Exp env t
  L :: Exp (a, env) b        -> Exp env (a -> b)
  A :: Exp env      (a -> b) -> Exp env a        -> Exp env b
deriving instance Show (Exp env t)

lookp :: Var env t -> env -> t
lookp (VS v) (_, xs) = lookp v xs
lookp V0     (x, _)  = x

interp :: Exp env t -> env -> t
interp (B b)    _   = b
interp (V var)  env = lookp var env
interp (L body) env = \arg -> interp body (arg, env)
interp (A f a)  env = (interp f env) (interp a env)

eval exp = interp exp ()

-- (λ x y . x) #t #f
prog0 = A (A (L (L (V (VS V0))))
             (B True))
          (B False)

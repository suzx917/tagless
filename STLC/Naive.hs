{-# LANGUAGE GADTs,
             DeriveFunctor,
             StandaloneDeriving
#-}

module STLC.Naive where

import Data.Map as M

data Expr where
  Int :: Integer -> Expr
  Var :: String -> Expr
  Lam :: String -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  -- adding more cases introduces boilerplate, similar code compared to `App`
  Add :: Expr -> Expr -> Expr
  IfZ :: Expr -> Expr -> Expr -> Expr
  deriving (Show, Eq)

data Val where
  VInt  :: Integer -> Val
  VClos :: String -> Expr -> Env -> Val
  deriving (Show, Eq)

type Env = M.Map String Val

extendEnv var arg env =
  M.insert var arg env
  
applyClos (VClos x body env) arg =
  interp body (extendEnv x arg env)

interp :: Expr -> Env -> Maybe Val
interp (Int n)      _   = return $ VInt n
interp (Var x)      env = M.lookup x env
interp (Lam x body) env = return $ VClos x body env
interp (App f a)    env = do
  rator <- interp f env
  rand  <- interp a env
  applyClos rator rand
-- xval and yval need to be unpacked to ints, annoyingly
interp (Add x y)   env  = do
  xval <- interp x env
  yval <- interp y env
  xint <- unpack xval
  yint <- unpack yval
  return $ VInt (xint + yint)
interp (IfZ cond thn els) env = do
  cval <- interp cond env
  cint <- unpack cval
  if cint == 0
    then interp thn env
    else interp els env

unpack (VInt n) = Just n
unpack x        = error $ show x ++ " is not VInt"

eval t = interp t M.empty

-- (λ x. λ y. x) 0 1 == 0
prog0 = App (App (Lam "x" (Lam "y" (Var "x")))
                 (Int 0))
            (Int 1)

iden = Lam "x" (Var "x")

-- Z operator
fix = let m = Lam "z" (App (App (Var "y") (Var "y")) (Var "z"))
          n = Lam "y" (App (Var "f") m)
       in Lam "f" (App n n)

mul' = Lam "x" (Lam "f" (Lam "y"
        (IfZ (Var "y")
          (Int 0)
          (Add (Var "x") (App (Var "f") (Add (Var "y") (Int (-1))))))))

mul = App fix mul'

-- 3 * 4
prog1 = App (App fix (App mul' (Int 3))) (Int 4)


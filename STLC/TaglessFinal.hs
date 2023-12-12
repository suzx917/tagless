-- STLC with Bool, tagless final
-- from Oleg's tutorial

-- without NoMono.. programs throw ambiguous type variable error
{-# LANGUAGE NoMonomorphismRestriction #-} 
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE RankNTypes, KindSignatures, TypeFamilies #-}

module STLC.TaglessFinal where

class Symantics repr where
  v0  :: repr (a, env) a
  vs  :: repr env a -> repr (any, env) a
  lam :: repr (a, env) b -> repr env (a -> b)
  -- lam ::  (repr a -> repr b) -> repr (a -> b) -- using HOAS
  app :: repr env (a -> b) -> repr env a -> repr env b

  int :: Integer -> repr env Integer
  add :: repr env Integer -> repr env Integer -> repr env Integer

-- 2 + 1 = 3
prog0 = add (int 2) (int 1)

-- (λ λ 1) (Int 0) (Int 1) = Int 0
--prog1 :: Symantics repr => repr env Integer
prog1 = app (app (lam (lam (vs v0)))
                 (int 0))
            (int 1)

prog2 :: Symantics repr => repr env (b -> b)
prog2 = lam v0

newtype R env a = R { unR :: env -> a }

instance Symantics R where
  v0   = R $ \(x, xs) -> x
  vs v = R $ \(_, xs) -> (unR v) xs
  
  lam body = R $ \xs -> \x -> (unR body) (x , xs)
  app f g  = R $ \xs -> (unR f) xs $ (unR g) xs

  int n   = R $ \xs -> n
  add a b = R $ \xs -> (unR a) xs + (unR b) xs

eval p = unR p ()

-- ^how to make this work without wrapping with newtype and a TAG?
--   just use `->`
instance Symantics (->) where
  v0     (x, xs) = x
  (vs v) (_, xs) = v xs
  
  lam body xs    = \x -> body (x , xs)
  app f    g     = \env -> (f env) (g env)

  int n   = \_   -> n
  add a b = \env -> a env + b env

eval' p = p ()

-- pretty print
newtype S h a = S { unS :: Int -> String }

instance Symantics S where
  v0        = S $ \h -> "x" ++ show (h - 1)
  vs v      = S $ \h -> unS v (h - 1)
  lam e     = S $ \h ->
    let x = "x" ++ show h
     in "(\\" ++ x ++ " -> " ++ unS e (h + 1) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

  int x     = S $ const $ show x
  add e1 e2 = S $ \h -> "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"

view p = unS p 0 

-- ^how to define S without the wrapper?





{-
newtype CountV0 env a = CountV0 { unwrappCountv0 :: Int }

instance Symantics CountV0 where
  v0       = 1
  vs v     = v
  lam body = body
  app f g  = f + g
  int      = 0
  add      = 0

--countV0' :: forall repr env a. Symantics repr => repr env a -> CountV0
--countV0' = undefined

countV0 :: forall repr env a. Symantics repr => repr env a -> Int
countV0 = undefined

blabla :: (forall repr env a. Symantics repr => repr env a) -> Int
blabla x = unwrapCountv0 (x @CountV0)

countV0'' :: CountV0 -> CountV0
countV0'' = id
-}


-- printProfileStats :: Symantics repr => repr env a -> IO ()
-- printProfileStats x = 

-- asst = countV0'' (prog0 @CountV0) == 1

-- countV0 (e :: Eval) -- legal
-- blabla (e :: Eval) -- not

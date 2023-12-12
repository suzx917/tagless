{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE StandaloneDeriving   #-}
module STLC.FreeMonad where

import Data.Functor.Identity
{-
-- 1. Free Monad Library
-}
data Free f a = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Impure ffa) = Impure $ fmap (fmap g) ffa

instance Functor f => Applicative (Free f) where
  pure = Pure
  
  (Pure g) <*> ffa = fmap g ffa
  (Impure ffg) <*> ffa = Impure $ fmap (<*> ffa) ffg

instance Functor f => Monad (Free f) where
  (Pure a) >>= h = h a
  (Impure ffa) >>= h = Impure $ fmap (>>= h) ffa

join :: Monad m => m (m a) -> m a
join = (>>= id)

-- Oleg calls it `eta` 
liftF :: Functor f => f a -> Free f a
liftF f = Impure (fmap pure f)

-- "given a natural transformation, returns a monad homomorphism"
-- Note: the library function doesn't have `Functor` constraint on `f`
foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _   (Pure a) = return a
foldFree phi (Impure ffa) = join $ phi (fmap (foldFree phi) ffa)

{-
-- 2. A Simple Calculator Example
--    from https://serokell.io/blog/introduction-to-free-monads

data CalF t r where
  Lit    :: Int -> (Int -> r) -> CalF t r
  Plus   :: t -> t -> (t -> r) -> CalF t r
  Input  :: (t -> r) -> CalF t r
  Output :: t -> (() -> r) -> CalF t r
  deriving Functor
-}
-- fixed Plus arguments to Int,
-- the result type cannot be (CalF Int r) in order to derive functor (CalF t)
-- the continuation boilerplate should be taken care of by Freer Monad
data CalF t r where
  Lit    :: Int -> (Int -> r) -> CalF t r
  Plus   :: Int -> Int -> (Int -> r) -> CalF t r
  Input  :: (t -> r) -> CalF t r
  Output :: t -> (() -> r) -> CalF t r
  deriving Functor

type Cal t = Free (CalF t)

-- term constructors, can be automated by `makeFree` from
-- https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free-TH.html
lit :: Int -> Cal Int Int
lit x = liftF $ Lit x id

plus :: Int -> Int -> Cal Int Int
plus x y = liftF $ Plus x y id

input :: Cal t t
input = liftF $ Input id

output :: t -> Cal t ()
output x = liftF $ Output x id

-- program: takes input x => output x-1
ex1 :: Cal Int ()
ex1 = do
  a <- lit (-1)
  x <- input
  r <- plus x a
  output r

calculateF :: CalF Int x -> IO x
calculateF (Lit x k)    = return $ k x
calculateF (Plus x y k) = return $ k (x + y)
calculateF (Input k)    = do { x <- read <$> getLine ; return $ k x }
calculateF (Output x k) = do { print x ; return $ k ()  }

calculate :: Cal Int x -> IO x
calculate = foldFree calculateF

{-
-- trying not to use continuations
data CalF' r where
  Lit' :: r -> CalF' r
  Plus' :: r -> r -> CalF' r
  Input' :: CalF' r
  Output' :: r -> CalF' r
deriving instance Functor CalF'

type Cal' = Free CalF'

lit' = liftF . Lit'
plus' x y = liftF $ Plus' x y
input' = liftF Input'
output' x = liftF $ Output' x


calculateF' :: CalF' x -> IO x
calculateF' (Lit' x)    = return x
-- ^lit x fixes the type var to Int
calculateF' (Plus' x y) = return (x + y)
calculateF' (Input')    = do { x <- read <$> getLine ; return x }
calculateF' (Output' x) = do { print x ; return 0  }
  -- ^here we are forced to return a number

-- foldFree does not work because calculateF' is not polymorphic
calculate' = foldFree calculateF'
-}


-- 3.1 STLC using Free Monad (HOAS)

-- Underlying functor
data ExpF t r where
  Lam :: (a -> b) -> ((a -> b) -> r) -> ExpF t r
  App :: (a -> b) -> a -> (b -> r) -> ExpF t r

  Int :: Integer -> (Integer -> r) -> ExpF t r
  Add :: t -> t -> (t -> r) -> ExpF t r

  Print :: Show t => t -> (() -> r) -> ExpF t r
deriving instance Functor (ExpF t)

type Exp t = Free (ExpF t)

-- term constructors
lam f = liftF $ Lam f id
app f x = liftF $ App f x id
int x = liftF $ Int x id
add x y = liftF $ Add x y id
print' x = liftF $ Print x id

add1 = lam $ \x -> x + 1

-- example program (add1 1 => 2)
-- notice it's written in monadic style, instead of a tree like expression
-- this forces the programmer to linearize the evaluation order.
-- the immediate problem with this is we lost access to the tree like ASTree
prog1 = do
  f <- add1
  x <- int 1
  y <- app f x
  print' y

-- interpreter
evalF :: (Show t, Num t) => ExpF t r -> IO r
evalF (Lam f k) = return $ k f
evalF (App f x k) = return $ k (f x)
evalF (Int n k) = return $ k n
evalF (Add x y k) = return $ k (x + y)
evalF (Print x k) = do { print x ; return $ k () }

eval = foldFree evalF


-- 3.2 STLC using Free Monad
-- allows writing programs in a exp tree style,
-- instead of forcing it to be monadic/linearized

data ExpF' t r where
  Lam' :: (a -> Exp' t b) -> ((a -> Exp' t b) -> r) -> ExpF' t r
  App' :: Exp' t (a -> Exp' t b) -> Exp' t a -> (b -> r) -> ExpF' t r

  Int' :: Integer -> (Integer -> r) -> ExpF' t r
  Add' :: Num t => Exp' t t -> Exp' t t -> (t -> r) -> ExpF' t r

  Print' :: Show t => Exp' t t -> (() -> r) -> ExpF' t r
deriving instance Functor (ExpF' t)

type Exp' t = Free (ExpF' t)

-- term constructors
lam' f = liftF $ Lam' f id
app' f x = liftF $ App' f x id
int' x = liftF $ Int' x id
add' x y = liftF $ Add' x y id
print'' x = liftF $ Print' x id

 
add1' :: Num t => Exp' t (t -> Exp' t t)
add1' = lam' $ \x -> return (x+1)

-- (add1 (2 + 3)) = 6
prog2 = app' add1' (add' (int' 2) (int' 3))
prog3 = print'' prog2

{-
-- interpreter to Identity monad
-- prog2 => Identity 6
-- prog3 => Identity ()

evalF' :: (Show t, Num t) => ExpF' t r -> Identity r
evalF' (Lam' f k) = return $ k f
evalF' (App' rator rand k) = do
  x <- eval' rand
  f <- eval' rator
  y <- eval' $ f x
  return $ k y
evalF' (Int' n k) = return $ k n
evalF' (Add' e1 e2 k) = do
  x <- eval' e1
  y <- eval' e2
  return $ k (x + y)
evalF' (Print' x k) = do
  x' <- eval' x
  return $ k ()

eval' :: (Show t, Num t) => Exp' t r -> Identity r
eval' = foldFree evalF'
-}

-- interpreter to IO monad
evalF' :: (Show t, Num t) => ExpF' t r -> IO r
evalF' (Lam' f k) = return $ k f
evalF' (App' rator rand k) = do
  x <- eval' rand
  f <- eval' rator
  y <- eval' $ f x
  return $ k y
evalF' (Int' n k) = return $ k n
evalF' (Add' e1 e2 k) = do
  x <- eval' e1
  y <- eval' e2
  return $ k (x + y)
evalF' (Print' x k) = do
  x' <- eval' x
  print x'
  return $ k ()

eval' :: (Show t, Num t) => Exp' t r -> IO r
eval' = foldFree evalF'

{-
-- pprinter
pprintF' :: (Show t, Num t) => ExpF' t r -> IO r
-- higher order
pprintF' (Lam' f k) = do
  print $ "(lam "
  
  return $ k f
pprintF' (App' rator rand k) = do
  x <- pprint' rand
  f <- pprint' rator
  y <- pprint' $ f x
  return $ k y
pprintF' (Int' n k) = return $ k n
pprintF' (Add' e1 e2 k) = do
  x <- pprint' e1
  y <- pprint' e2
  return $ k (x + y)
pprintF' (Print' x k) = do
  x' <- pprint' x
  print x'
  return $ k ()

pprint' :: (Show t, Num t) => Exp' t r -> IO r
pprint' = foldFree pprintF'
-}

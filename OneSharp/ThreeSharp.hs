{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

-- Using Tagless Final approach

module ThreeSharp where

import Data.Map as M

import Queue
import Parser

class Instr r where
  addO :: Integer -> r
  addS :: Integer -> r
  shiftF :: Integer -> r
  shiftB :: Integer -> r
  cases  :: Integer -> r

-- Concrete datatype for pattern matching in Machine step instance
data Instruction where
  AddO :: Integer -> Instruction
  AddS :: Integer -> Instruction
  ShiftF :: Integer -> Instruction
  ShiftB :: Integer -> Instruction
  Cases  :: Integer -> Instruction

instance Instr Instruction where
  addO = AddO
  addS = AddS
  shiftF = ShiftF
  shiftB = ShiftB
  cases = Cases
{-
class Instr (Elem a) => Machine a where
  type Elem a
  load :: [Elem a] -> Register -> a
  shift  :: Integer -> a -> a
  regOut :: a -> Register
  regIn  :: Register -> a -> a
  step :: a -> a
  isHalt :: a -> Bool
-}

type Register = M.Map Integer String

-- It seems necessary to expose the type variable of Instr and Queue in
-- the type annotation of the Machine class
-- TODO: compared to hiding the information using type family


{-
-- Attemps

-- This is not allowed by haskell, need to use type families
--class Instr a => Machine ([a],[a],Register) where

-- type family example
--class Instr (Elem a) => Machine a where
--  type Elem a

--instance Instr (Elem a) => Machine ([a],[a],Register) where
--  type Elem ([a],[a],Register) = a

-- only use constructors to get "initial" equivalent definition
class Instr a => Machine m a where
  machine :: [a] -> [a] -> Register -> m a
-}


type AbsInstr = forall a. Instr a => a


-- Machine needs to depend on the abstract Instr type, not instaces of Instr
-- and we define the class as a queue of instructions,
-- instiante as Reg -> Reg
class Machine m where
  machine :: [AbsInstr] -> [AbsInstr] -> m


shiftq 0 _          = error "shifting 0"
shiftq 1 (back, []) = error "shifting out-of-bound"
shiftq 1 (back, (ins:rest)) = (ins:back, rest)
shiftq n (back, front)
 | n > 1 = shiftq (n-1) $ shiftq 1 (back, front)
 | n < 0 = swap $ shiftq (-n) (back, front)

-- how to achieve one step reduction??
instance Machine (Register -> Register) where
  machine _ []         reg = reg
  machine back (ins:rest) reg =
    case (ins :: Instruction) of -- is there a way to avoid
                                 -- instantiating Instr to concrete data here?
      AddO rn  -> let reg' = M.adjust (++ "1") rn reg
                   in machine (ins:back) rest reg'
      AddS rn  -> let reg' = M.adjust (++ "#") rn reg
                   in machine (ins:back) rest reg'
      ShiftF j -> let (b,f) = shiftq j (back,ins:rest)
                   in machine b f reg
      ShiftB j -> let (b,f) = shiftq (-j) (back,ins:rest)
                   in machine b f reg
      Cases rn -> case M.lookup rn reg of
        Nothing      -> error "cases lookup out-of-bound"
        Just ('1':w) -> let reg' = M.insert rn w reg
                            (b,f) = shiftq 2 (back,ins:rest)
                         in machine b f reg'
        Just ('#':w) -> let reg' = M.insert rn w reg
                            (b,f) = shiftq 3 (back,ins:rest)
                         in machine b f reg'
        _            -> error $ "illegal register value: R" ++ show rn " = " ++ show w
                           
instance Instr (Integer -> String) where
  addO  rn = \_   -> "Add 1 to R" ++ show rn
  addS  rn = \_   -> "Add # to R" ++ show rn
  shiftF i = \idx -> "Foward to line " ++ show (idx + i)
  shiftB i = \idx -> "Back to line " ++ show (idx - i)
  cases rn = \_   -> "Case split on R" ++ show rn


-- step instance
  
-- pprint
--instance Machine String where
-- it's weird to write `regOut` or `isHalt` here   



{-
id' :: forall a. a -> a
id' x = x

-- impredicative types
ids :: [forall a. a -> a]
ids = repeat id'
-}

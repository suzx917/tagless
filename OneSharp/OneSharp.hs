{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies,
             FlexibleInstances, GADTs,
             InstanceSigs, LambdaCase
#-}
module OneSharp where

import Data.Map as M
import Control.Monad (guard)
import Control.Monad.State.Lazy
import Parser

data Alphabet = O | S
type Text = [Alphabet]

type Stack = [Instr]
type Register = M.Map Integer String
type Machine = (Stack, Stack, Register)

data Instr
  = AddO Integer
  | AddS Integer
  | ShiftF Integer
  | ShiftB Integer
  | Cases  Integer
  | End              -- marks the proper exit of the program
  | Nop              -- for empty lines while shifting
  deriving (Show, Eq)

-- read number of ones and sharps
pOnes = counter (pChar '1')
pSharps = counter (pChar '#')

pInstr :: Parse Instr
pInstr = do
  os <- pOnes
  ss <- pSharps
  guard (os > 0 && ss > 0 && ss < 6) -- grammar check
  return $ (decode ss) os
  where
    decode = \case {1->AddO; 2->AddS; 3->ShiftF; 4->ShiftB; 5->Cases}

pProg :: Parse [Instr] 
pProg = star pInstr

parser = parse pProg

printInstr (AddO n)   _   = "Add 1 to R" ++ show n
printInstr (AddS n)   _   = "Add # to R" ++ show n
printInstr (ShiftF n) idx = "Foward to line " ++ show (idx + n)
printInstr (ShiftB n) idx = "Back to line " ++ show (idx - n)
printInstr (Cases n)  _   = "Case split on R" ++ show n
printInstr End       _    = "■"

--printProg :: Stack -> String
printProg stack = fst $ runState (traverse g stack) 1
    where
      g :: Instr -> State Integer String
      g ins = do
        idx <- get
        put (idx+1)
        return ("[" ++ show idx ++ "] " ++ printInstr ins idx ++ "\n")

    

shift :: Integer -> Machine -> Machine
shift n
  | n < 0 = s . shift (-n) . s    -- shifting backwards
  where
    s (back, front, reg) = (front, back, reg)
shift 0 = id
shift 1 = \(back, front, reg) ->
            case front of
              []  ->  (Nop:back, [], reg)      -- extend automatically
              h:t ->  (h:back  , t , reg)
shift n = shift (n-1) . shift 1

step :: Machine -> Machine
step mac@(back, front, reg) =
  case front of
    [] -> error $ "No instruction after " ++ show back
    AddO n   : rest -> (AddO n: back, rest, M.adjust (++ "1") n reg)
    AddS n   : rest -> (AddS n: back, rest, M.adjust (++ "#") n reg)
    ShiftF n : _    -> shift n    mac
    ShiftB n : _    -> shift (-n) mac
    Cases n  : rest ->
      case M.lookup n reg of
        Just ('1': cs) -> shift 2 (back, front, M.insert n cs reg)
        Just ('#': cs) -> shift 3 (back, front, M.insert n cs reg)
        _              -> shift 1 mac -- just go to next instr
    End : _        -> mac
    Nop : _        -> error $ "No instruction after " ++ show back

stepN 0 m = m
stepN 1 m = step m
stepN n m = stepN (n-1) (step m)

run :: Machine -> Machine
run m = let m' = step m in if m' == m then m' else run m'

{-
isHalt :: Machine -> Bool
isHalt (back, front, reg) =
  let l = toList reg
  front == [End] && length l == 1 && 
-}

load :: String -> Register -> Machine
load text reg = case parser text of
  Just ("", prog) -> ([], prog ++ [End], reg)

eval :: String -> Register -> Machine
eval text reg = run $ load text reg


-- Tests

-- concat R1 and R2
text0 = "11#####111111###111###1##1111####1#111111####"
prog0 = case parser text0 of
  Just ("", prog) -> prog ++ [End]
reg0 = fromList [(1, "1"),(2, "###")]
pp0   = printProg prog0

mac0 = ([], prog0, reg0)
res0 = run ([], prog0, reg0)
-- 


class Instr1 repr where
  addO   :: Integer -> repr -- add 1 to Rn
  addS   :: Integer -> repr -- add # to Rn
  shiftF :: Integer -> repr -- go forward n
  shiftB :: Integer -> repr -- go backward n
  cases  :: Integer -> repr -- cases on Rn
                            --   empty -> forward 1
                            --   starts with 1 -> delete it and forward 2
                            --   starts with # -> delete it and forward 3

class Instr1 repr => Instr' repr where
  bomb :: repr -> repr

--newtype S repr = Instr' repr => S { unS :: [repr] }


-- class Program' a where
--   type Repr a
--   runProg :: [Repr a] -> [Repr a]

-- instance Program' [a] where
--   type Repr a = 

-- class (Instr1 a, b ~ [a]) => Program' b where
--   runProg :: b -> b

class Instr' (Elem a) => Program' a where
  type Elem a
  runProg :: a -> a
  
instance (Instr' b) => Program' [b] where
  type Elem [b] = b
  runProg :: [b] -> [b]
  runProg = undefined


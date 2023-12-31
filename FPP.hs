-- following Oleg's tutorial
-- https://okmij.org/ftp/tagless-final/course/PrintScanF.hs

{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE StandaloneDeriving        #-}
--{-# LANGUAGE DeriveFunctor             #-}

module FPP where

import Prelude hiding ((^))

-- usal parser type is defined like this:
type Parser a = String -> Maybe (a, String)
type Printer a = a -> String

-- repr a b means: extending result type a with current continuation b.
-- for pprinting the result type eventually resolves to String,
-- for parsing, it resolves to 
class FormatSpec repr where
  lit  :: String -> repr a a
  int  :: repr a (Int -> a)
  char :: repr a (Char -> a)
  fpp  :: PrinterParser b -> repr a (b -> a) -- given `PP b` we can make a continuation from b
  (^)  :: repr b c -> repr a b -> repr a c
  -- concat the telescopic type vars:
  -- combine (b extends from a) and (c extends from b), get (c extends from a)

data PrinterParser a = PrinterParser (a -> String)
                                     (String -> Maybe (a, String))

-- Some helpers:
fmt :: (FormatSpec repr, Show b, Read b) => b -> repr a (b -> a)
fmt _ = fpp showread

-- casting `read` result from list to maybe
showread :: (Show a, Read a) => PrinterParser a
showread =
  PrinterParser show $
    \str -> case reads str of
      [(a, str')] -> Just (a, str')
      _ -> Nothing

-- format template
fmt1 = lit "Hello " ^ char ^ int

-- PPrinter
newtype FPr a b = FPr ((String -> a) -> b)
-- !! ^this form looks like CPS monad with different `r` types

sprintf :: FPr String b -> b
sprintf (FPr format) = format id
-- here we unify result type `a` with String

instance FormatSpec FPr where
    lit str = FPr $ \k -> k str
    -- these three are of the same form:
    int     = FPr $ \k x -> k (show x)
    char    = FPr $ \k x -> k [x]
    fpp (PrinterParser pr _) = FPr $ \k x -> k (pr x)
    -- monadic bind???
    (FPr a) ^ (FPr b)  = FPr $ \k -> a (\sa -> b (\sb -> k (sa ++ sb)))

ex0 = sprintf fmt1

print1 = sprintf fmt1 'm' 13
print2 = sprintf fmt1 'w' 23


-- Parser

-- Note: implemented myself given the pprinter example
newtype FSc a b = FSc (String -> b -> Maybe (a, String))

-- it's just unFSc
sscanf :: FSc a b -> String -> b -> Maybe (a, String)
sscanf (FSc format) str k = format str k 

removePrefix :: String -> String -> Maybe String
removePrefix long [] = Just long
removePrefix []   _  = Nothing
removePrefix (x:xs) (y:ys)
 | x == y    = removePrefix xs ys
 | otherwise = Nothing

instance FormatSpec FSc where
  lit str = FSc $ \inp k ->
    case removePrefix inp str of
      Just suffix -> Just (k, suffix)
      Nothing -> Nothing
  char = FSc $ \inp k ->
    case inp of
      (x : xs) -> Just (k x, xs)
      _        -> Nothing
  fpp (PrinterParser _ par) = FSc $ \inp k ->
    case par inp of
      Just (b, inp') -> Just (k b, inp')
      Nothing        -> Nothing
  int = fpp showread
  (FSc pl) ^ (FSc pr) = FSc $ \inp k ->
    case pl inp k of
      Just (k', inp') -> pr inp' k'
      _               -> Nothing

-- try "Hello 1a" would throw error
--fmt0 = lit "Hello " ^ char
str1 = "Hello a1"
parse1 = sscanf fmt1 str1 (,)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module STLC.FreeMonad where

import Data.List

data ExpF t r where
  Lam :: ExpF a r -> ExpF b r -> ExpF (a -> b) r
  App :: ExpF (a -> b) r -> ExpF a r -> ExpF b r

  Int :: Integer -> ExpF Integer r
  Add :: ExpF Integer r -> ExpF Integer r -> ExpF Integer r



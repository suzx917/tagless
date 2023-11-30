{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Queue where

{-
class Queue q where
  type Elem q
  enqueue :: Elem q -> q -> q
  dequeue :: q -> (q, Maybe (Elem q))

type TwoStacks a = ([a], [a])

instance Queue ([a], [a]) where
  type Elem ([a], [a]) = a
  
  enqueue a (s1, s2) = (a:s1, s2)
  
  dequeue q@([], [])     = (q, Nothing)
  dequeue   (s1, [])     = dequeue ([], reverse s1)
  dequeue   (s1, top:s2) = ((s1,s2), Just top)

-}

class Queue q where
  enqueue :: a -> q a -> q a
  dequeue :: q a -> (q a, Maybe a)

newtype TwoStacks a = TS { unTS :: ([a],[a]) }
instance Queue TwoStacks where
  enqueue a (TS (en, de)) = TS (a:en, de)

  dequeue q@(TS ([],[])) = (q,Nothing)
  dequeue (TS (en,[]))   = dequeue $ TS ([],en)
  dequeue (TS (en,a:de)) = (TS (en,de), Just a)


emptyQ = TS ([], [])

--q0 :: TwoStacks Int
q0 = enqueue 1 emptyQ

q0' = dequeue q0


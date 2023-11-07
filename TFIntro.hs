{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language NoMonomorphismRestriction #-}

module TFIntro where

{-
-- Initial
-}
data Exp = Lit Int | Neg Exp | Add Exp Exp

interp :: Exp -> Int
interp (Lit x)   = x
interp (Neg x)   = -(interp x)
interp (Add x y) = interp x + interp y

pprint :: Exp -> String
pprint (Lit x)   = show x
pprint (Neg x)   = "-(" ++ pprint x ++ ")"
pprint (Add x y) = "(" ++ pprint x ++ "+" ++ pprint y ++ ")"

-- You might also want these:
-- parser    :: Exp -> String -> Maybe (String, Exp)
-- eq        :: Exp -> Bool
-- typeCheck :: Exp -> Either TypeError Exp

{-
-- Tagless Final:
-}

-- parametrize over result type ~ giving "denotational semantics"
class SymAdd repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

-- notice I don't have to dispatch the evaluation to subexps
instance SymAdd Int where
  lit = id
  neg x = -x
  add x y = x + y

instance SymAdd String where
  lit = show
  neg x = "(-" ++ x ++ ")"
  add x y = "(" ++ x ++ "+" ++ y ++ ")"

-- 3 + (-4 + 5)
-- prog0 :: SymAdd repr => repr
prog0 :: SymAdd repr => repr
prog0 = add (lit 3) (add (neg (lit 4)) (lit 5))

-- evaluation ~ choosing an instance
eval :: Int -> Int
eval = id

view :: String -> String
view = id

-- extending the DSL
class MulSym repr where
  mul :: repr -> repr -> repr

-- 5 + -(2 * 3)
prog1 = add (lit 5) (neg (mul (lit 2) (lit 3)))

-- 3 * prog0
prog2 = mul (lit 3) prog0

instance MulSym Int where
  mul = (*)

instance MulSym String where
  mul x y = "(" ++ x ++ "*" ++ y ++ ")"


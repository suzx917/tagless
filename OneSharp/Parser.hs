{-# LANGUAGE LambdaCase, DeriveTraversable,
             StandaloneDeriving #-}

module Parser where

import Control.Applicative

{-
class Letter l where
  oneL :: l
  sharpL :: l

instance Letter Char where
  oneL = '1'
  sharpL = '#'

instance Letter String where
  oneL = "1"
  sharpL = "#"
-}

newtype Parse a = P {parse :: String -> Maybe (String, a)}
deriving instance Functor Parse

instance Applicative Parse where
  pure a = P $ \s -> Just (s, a)
  (P pf) <*> (P pa) =
    P $ \s -> do
      (s', f) <- pf s
      (s'', a) <- pa s'
      return (s'' , f a)

instance Monad Parse where
  (P pa) >>= f = P $ \s -> do
    (s', a) <- pa s
    parse (f a) s'

instance Alternative Parse where
  empty = P $ \_ -> Nothing
  (P pa) <|> (P pb) =
    P $ \s -> pa s <|> pb s

pChar :: Char -> Parse Char
pChar c = P $ \case {x:rest -> if x == c then Just (rest, c) else Nothing;
                     _      -> Nothing}

pWord :: String -> Parse String
pWord = traverse pChar

instance Semigroup a => Semigroup (Parse a) where
  (P pa) <> (P pb) =
    P $ \s -> do
      (s', a) <- pa s
      (s'', b) <- pb s'
      return (s'', a <> b)

-- wrt. concat
instance Monoid a => Monoid (Parse a) where
  mempty = return mempty

star :: Parse a -> Parse [a]
star p = (:) <$> p <*> star p <|> mempty

positive :: Parse a -> Parse [a]
positive p = (:) <$> p <*> star p

counter :: Parse a -> Parse Integer
counter p = P $ \s ->
  case parse p s of
    Nothing -> return (s, 0)
    Just (s', _) -> (\(str, n) -> (str, n+1)) <$> parse (counter p) s'

-- test0 = parse (star (pWord "c")) "cc0"


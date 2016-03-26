-- | Various utilities to support SavedIO.

module SavedIO.Util
( if'
, (?)
, optional
) where

import            Control.Applicative             (Alternative, (<|>))
import            Data.Optional                   (Optional(..))

-- |Functional alternative to if-then-else.
-- See https://wiki.haskell.org/If-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-- |C-like ternary operator.
--  Usage: cond ? exp1 $ exp2
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

optional :: Alternative f => f a -> f (Optional a)
optional v = Specific <$> v <|> pure Default

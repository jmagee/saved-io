-- | Various utilities to support SavedIO.

module SavedIO.Util
( if'
, (?)
, optional
, perhaps
) where

import            Control.Applicative             (Alternative, (<|>))
import            Data.Optional                   (Optional(..))

-- | Functional alternative to if-then-else.
-- See https://wiki.haskell.org/If-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-- | C-like ternary operator.
--  Usage: cond ? exp1 $ exp2
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

-- | Equivalent to optional from Control.Applicative, but return Data.Optional
-- instead of Maybe.
optional :: Alternative f => f a -> f (Optional a)
optional v = Specific <$> v <|> pure Default

-- | The perhaps function takes a default value, a function, and an Optional
-- value.  If the Optional value is Default, the function returns the default
-- value.  Otherwise, it applies the function the value inside the Specific and
-- returns the result.
--
-- Like the function maybe but for Optional.
perhaps :: b -> (a -> b) -> Optional a -> b
perhaps def _ Default      = def
perhaps _   f (Specific x) = f x

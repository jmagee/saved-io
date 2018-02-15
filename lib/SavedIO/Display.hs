-- Display.hs
-- Typeclass that provides conversion to string (like show), but with the intention to be
-- read by a user (unlike show, which ought to output something that can be parsed with read.)

-- These are needed to define type instances on String.
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}

module SavedIO.Display
( Display
, display
) where

-- | A type class for displaying data
-- Like show, but intended to be user derived to have better
-- control of the final string.
class Show a => Display a where
  display :: a -> String
  display = show

-- instance Display String where
--  display x = x

{-instance Display a => Display (Maybe a) where-}
  {-display (Just x) = display x-}
  {-display Nothing  = "Unspecified"-}

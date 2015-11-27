{-# LANGUAGE Arrows #-}

module Arrow.ArrowPlus (
    ArrowZero(..),
    ArrowPlus(..),
    many,
    some,
    unfold
    )
where

import Prelude ()

import Control.Arrow ( Arrow, returnA )
import Control.Category ( Category(..), (>>>) )
import Data.Monoid

-- Operations respecting:
--      f <+> zero = f
--   zero <+> f    = f
--   zero >>> f    = zero
--      f >>> zero = zero

class ArrowZero a where
    zero        :: a b c

class ArrowPlus a where
    (<+>)       :: a b c -> a b c -> a b c

-- The following may better be expressed as _loops_

many :: (Category a, ArrowPlus a) => a i i -> a i i
many f =
    some f <+> id

some :: (Category a, ArrowPlus a) => a i i -> a i i
some f = f >>> many f

-- similar to 'many', accumulate using a Monoid
unfold :: (Arrow a, ArrowPlus a, Monoid m) => a () m -> a () m
unfold f = proc () -> do
    many g -< mempty
    where
    g = proc x -> do
        fx <- f -< ()
        returnA -< x `mappend` fx
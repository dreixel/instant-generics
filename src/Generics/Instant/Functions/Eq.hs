
-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Eq
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The equality function.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Eq (Eq(..), eq) where

import Generics.Instant.Base
import Generics.Instant.Instances ()

import Prelude hiding (Eq)

-- Generic eq on Representable (worker)
class Eq a where
  eq' :: a -> a -> Bool

instance Eq U where
  eq' U U = True
  
instance (Eq a, Eq b) => Eq (a :+: b) where
  eq' (L x) (L x') = eq' x x'
  eq' (R x) (R x') = eq' x x'
  eq' _      _     = False
  
instance (Eq a, Eq b) => Eq (a :*: b) where
  eq' (a :*: b) (a' :*: b') = eq' a a' && eq' b b'
  
instance (Eq a) => Eq (C c a) where
  eq' (C a) (C a') = eq' a a'

instance Eq a => Eq (Var a) where
  eq' (Var x) (Var x') = eq' x x'

instance (Eq a) => Eq (Rec a) where
  eq' (Rec x) (Rec x') = eq' x x'

-- Dispatcher
eq :: (Representable a, Eq (Rep a)) => a -> a -> Bool
eq x y = eq' (from x) (from y)


-- Adhoc instances
instance Eq Int      where eq' = (==)
instance Eq Integer  where eq' = (==)
instance Eq Float    where eq' = (==)
instance Eq Double   where eq' = (==)
instance Eq Char     where eq' = (==)
instance Eq Bool     where eq' = (==)

-- Generic instances
instance (Eq a) => Eq (Maybe a)    where eq' = eq
instance (Eq a) => Eq [a]          where eq' = eq
instance (Eq a, Eq b) => Eq (a, b) where eq' = eq

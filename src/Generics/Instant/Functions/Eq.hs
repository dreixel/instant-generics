{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Eq
-- Copyright   :  (c) 2011, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The equality function.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Eq (GEq(..), geqDefault) where

import Generics.Instant.Base
import Generics.Instant.Instances ()


-- Generic eq on Representable (worker)
class GEq' a where
  geq' :: a -> a -> Bool

instance GEq' U where
  geq' U U = True

instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L x) (L x') = geq' x x'
  geq' (R x) (R x') = geq' x x'
  geq' _      _     = False

instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a :*: b) (a' :*: b') = geq' a a' && geq' b b'

instance (GEq' a) => GEq' (CEq c p q a) where
  geq' (C a) (C a') = geq' a a'

instance GEq a => GEq' (Var a) where
  geq' (Var x) (Var x') = geq x x'

instance (GEq a) => GEq' (Rec a) where
  geq' (Rec x) (Rec x') = geq x x'


class GEq a where
  geq :: a -> a -> Bool

-- Dispatcher
geqDefault :: (Representable a, GEq' (Rep a)) => a -> a -> Bool
geqDefault x y = geq' (from x) (from y)


-- Adhoc instances
instance GEq Int      where geq = (==)
instance GEq Integer  where geq = (==)
instance GEq Float    where geq = (==)
instance GEq Double   where geq = (==)
instance GEq Char     where geq = (==)
instance GEq Bool     where geq = (==)

-- Generic instances
instance (GEq a) => GEq (Maybe a)     where geq = geqDefault
instance (GEq a) => GEq [a]           where geq = geqDefault
instance (GEq a, GEq b) => GEq (a, b) where geq = geqDefault

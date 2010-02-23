
-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Update
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic update function.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Update (Update(..), update, MapOn(..)) where

import Generics.Instant.Base
import Generics.Instant.Instances ()


-- Generic update on Representable (worker)
class Update a where
  update' :: a -> a

instance Update U where
  update' U = U
  
instance (Update a, Update b) => Update (a :+: b) where
  update' (L x) = L (update' x)
  update' (R x) = R (update' x)
  
instance (Update a, Update b) => Update (a :*: b) where
  update' (a :*: b) = update' a :*: update' b
  
instance (Update a, Constructor c) => Update (C c a) where
  update' (C a) = C (update' a)

instance Update a => Update (Rec a) where
  update' (Rec x) = Rec (update' x)

instance (MapOn a) => Update (Var a) where
  update' (Var x) = Var (mapOn x)

class MapOn a where
  mapOn :: a -> a
  mapOn = id


-- Dispatcher
update :: (Representable a, Update (Rep a)) => a -> a
update = to . update' . from


-- Adhoc instances


-- Generic instances
instance (MapOn a)          => Update (Maybe a) where update' = update
instance (MapOn a)          => Update [a]       where update' = update
instance (MapOn a, MapOn b) => Update (a,b)     where update' = update

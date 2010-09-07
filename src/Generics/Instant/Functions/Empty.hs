{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverlappingInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Empty
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generically produce a single finite value of a datatype.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Empty (
    Empty(..), empty,
    HasRec(..)
  ) where

import Generics.Instant.Base
import Generics.Instant.Instances ()

-- Generic empty on Representable (worker)
class Empty a where
  empty' :: a

instance Empty U where
  empty' = U
  
instance (HasRec a, Empty a, Empty b) => Empty (a :+: b) where
  empty' = if hasRec' (empty' :: a) then R empty' else L empty'
  
instance (Empty a, Empty b) => Empty (a :*: b) where
  empty' = empty' :*: empty'
  
instance (Empty a) => Empty (C c a) where
  empty' = C empty'

instance (Empty a) => Empty (Var a) where
  empty' = Var empty'

instance (Empty a) => Empty (Rec a) where
  empty' = Rec empty'

instance Empty Int where
  empty' = 0

instance Empty Integer where
  empty' = 0

instance Empty Float where
  empty' = 0

instance Empty Double where
  empty' = 0

instance Empty Char where
  empty' = '\NUL'
  
instance Empty Bool where
  empty' = False


-- Dispatcher
empty :: (Representable a, Empty (Rep a)) => a
empty = to empty'

-- Adhoc instances
-- none

-- Generic instances
instance (Empty a) => Empty (Maybe a)       where empty' = empty
instance (Empty a) => Empty [a]             where empty' = empty
instance (Empty a, Empty b) => Empty (a,b)  where empty' = empty


--------------------------------------------------------------------------------
-- | We use 'HasRec' to check for recursion in the structure. This is used 
-- to avoid selecting a recursive branch in the sum case for 'Empty'.
class HasRec a where
  hasRec' :: a -> Bool
  hasRec' _ = False
  
instance HasRec U
instance HasRec (Var a)

instance (HasRec a, HasRec b) => HasRec (a :*: b) where
  hasRec' (a :*: b) = hasRec' a || hasRec' b
  
instance (HasRec a, HasRec b) => HasRec (a :+: b) where
  hasRec' (L x) = hasRec' x
  hasRec' (R x) = hasRec' x

instance (HasRec a) => HasRec (C c a) where
  hasRec' (C x) = hasRec' x
  
instance HasRec (Rec a) where
  hasRec' _ = True
  
instance HasRec Int
instance HasRec Integer
instance HasRec Float
instance HasRec Double
instance HasRec Char

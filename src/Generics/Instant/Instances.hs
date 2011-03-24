{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS -fno-warn-orphans         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Instances
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines instances of the 'Representable' class for a number of
-- basic Prelude types.
--
-----------------------------------------------------------------------------

module Generics.Instant.Instances () where

import Generics.Instant.Base
  
instance Representable Int where 
  type Rep Int = Int
  to = id
  from = id
  
instance Representable Char where 
  type Rep Char = Char
  to = id
  from = id
  
instance Representable Bool where 
  type Rep Bool = Bool
  to = id
  from = id

instance Representable Float where 
  type Rep Float = Float
  to = id
  from = id
  
instance Representable U where 
  type Rep U = U
  to = id
  from = id

instance (Representable a, Representable b) => Representable (a :*: b) where 
  type Rep (a :*: b) = a :*: b
  to = id
  from = id

instance (Representable a, Representable b) => Representable (a :+: b) where 
  type Rep (a :+: b) = a :+: b
  to = id
  from = id
  
instance Representable a => Representable (C c a) where 
  type Rep (C c a) = C c a
  to = id
  from = id
  
instance Representable a => Representable (Var a) where 
  type Rep (Var a) = Var a
  to = id
  from = id

instance Representable a => Representable (Rec a) where 
  type Rep (Rec a) = Rec a
  to = id
  from = id

-- Lists
instance Representable [a] where
  type Rep [a] = C List_Nil_ U :+: C List_Cons_ (Var a :*: Rec [a])
  from []                           = L (C U)
  from (a:as)                       = R (C (Var a :*: Rec as))
  to (L (C U))                  = []
  to (R (C (Var a :*: Rec as))) = (a:as)

data List_Nil_
instance Constructor List_Nil_  where conName _ = "[]"
data List_Cons_
instance Constructor List_Cons_  where
  conName _   = ":"
  conFixity _ = Infix RightAssociative 5

-- Maybe
instance Representable (Maybe a) where
  type Rep (Maybe a) = C Maybe_Nothing_ U :+: C Maybe_Just_ (Var a)
  from Nothing           = L (C U)
  from (Just x)          = R (C (Var x))
  to (L (C U))       = Nothing
  to (R (C (Var x))) = Just x
  
data Maybe_Nothing_
instance Constructor Maybe_Nothing_  where conName _ = "Nothing"
data Maybe_Just_
instance Constructor Maybe_Just_  where conName _ = "Just"

-- Pairs
instance Representable (a,b) where
  type Rep (a,b) = C Tuple_Pair_ (Var a :*: Var b)
  from (a,b)                   = C (Var a :*: Var b)
  to (C (Var a :*: Var b)) = (a,b)


data Tuple_Pair_
instance Constructor Tuple_Pair_ where conName _ = "," -- Prefix?

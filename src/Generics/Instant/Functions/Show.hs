{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Show
-- Copyright   :  (c) 2011, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Simplified generic show function.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Show (GShow(..), gshowDefault) where

import Generics.Instant.Base
import Generics.Instant.Instances ()

import Data.List (intersperse)

-- Generic show on Representable (worker)
class GShow' a where
  gshow' :: a -> String

instance GShow' U where
  gshow' U = ""

instance (GShow' a, GShow' b) => GShow' (a :+: b) where
  gshow' (L x) = gshow' x
  gshow' (R x) = gshow' x

instance (GShow' a, GShow' b) => GShow' (a :*: b) where
  gshow' (a :*: b) = gshow' a `space` gshow' b

instance (GShow' a, Constructor c) => GShow' (CEq c p q a) where
  gshow' c@(C a) | gshow' a == "" = paren $ conName c
                 | otherwise      = paren $ (conName c) `space` gshow' a

instance GShow a => GShow' (Var a) where
  gshow' (Var x) = gshow x

instance GShow a => GShow' (Rec a) where
  gshow' (Rec x) = gshow x

class GShow a where
  gshow :: a -> String

-- Dispatcher
gshowDefault :: (Representable a, GShow' (Rep a)) => a -> String
gshowDefault = gshow' . from


-- Adhoc instances
instance GShow Int      where gshow = show
instance GShow Integer  where gshow = show
instance GShow Float    where gshow = show
instance GShow Double   where gshow = show
instance GShow Char     where gshow = show
instance GShow Bool     where gshow = show

instance GShow a => GShow [a] where
  gshow = concat . wrap "[" "]" . intersperse "," . map gshow

instance GShow [Char] where gshow = show

instance (GShow a, GShow b) => GShow (a, b) where
  gshow (a,b) = "(" ++ gshow a ++ "," ++ gshow b ++ ")"


-- Generic instances
instance (GShow a) => GShow (Maybe a) where gshow = gshowDefault


-- Utilities
space :: String -> String -> String
space a b = a ++ " " ++ b

paren :: String -> String
paren x = "(" ++ x ++ ")"

wrap :: a -> a -> [a] -> [a]
wrap h t l = h:l++[t]

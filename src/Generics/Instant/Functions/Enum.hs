{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Enum
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generically enumerate values
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Enum (
    GEnum(..), genum
  ) where

import Generics.Instant.Base
import Generics.Instant.Instances ()

-- Generic enum (worker)
class GEnum a where
  genum' :: [a]

instance GEnum U where
  genum' = [U]

instance (GEnum a) => GEnum (Rec a) where
  genum' = map Rec genum'

instance (GEnum a) => GEnum (Var a) where
  genum' = map Var genum'

instance (GEnum a) => GEnum (CEq c p p a) where genum' = map C genum'
instance              GEnum (CEq c p q a) where genum' = []

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  genum' = map L genum' ||| map R genum'

instance (GEnum f, GEnum g) => GEnum (f :*: g) where
  genum' = diag (map (\x -> map (\y -> x :*: y) genum') genum')


instance GEnum Int where
  genum' = [0..9]


-- Dispatcher
genum :: (Representable a, GEnum (Rep a)) => [a]
genum = map to genum'


-- Utilities
infixr 5 |||

(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

diag :: [[a]] -> [a]
diag = concat . foldr skew [] . map (map (\x -> [x]))

skew :: [[a]] -> [[a]] -> [[a]]
skew []     ys = ys
skew (x:xs) ys = x : combine (++) xs ys

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine _ xs     []     = xs
combine _ []     ys     = ys
combine f (x:xs) (y:ys) = f x y : combine f xs ys

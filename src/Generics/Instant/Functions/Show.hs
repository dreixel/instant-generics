
-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Functions.Show
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic show function.
--
-----------------------------------------------------------------------------

module Generics.Instant.Functions.Show (Show(..), show) where

import Generics.Instant.Base
import Generics.Instant.Instances ()

import Prelude hiding (Show, show)
import qualified Prelude as P (Show, show)
import Data.List (intersperse)

-- Generic show on Representable (worker)
class Show a where
  show' :: a -> String

instance Show U where
  show' U = ""
  
instance (Show a, Show b) => Show (a :+: b) where
  show' (L x) = show' x
  show' (R x) = show' x
  
instance (Show a, Show b) => Show (a :*: b) where
  show' (a :*: b) = show' a `space` show' b
  
instance (Show a, Constructor c) => Show (C c a) where
  show' c@(C a) | show' a == "" = paren $ conName c
                | otherwise     = paren $ (conName c) `space` show' a

instance Show a => Show (Var a) where
  show' (Var x) = show' x

instance Show a => Show (Rec a) where
  show' (Rec x) = show' x


-- Dispatcher
show :: (Representable a, Show (Rep a)) => a -> String
show = show' . from


-- Adhoc instances
instance Show Int      where show' = P.show
instance Show Integer  where show' = P.show
instance Show Float    where show' = P.show
instance Show Double   where show' = P.show
instance Show Char     where show' = P.show
instance Show Bool     where show' = P.show

instance Show a => Show [a] where
  show' = concat . wrap "[" "]" . intersperse "," . map show'

instance Show [Char] where
  show' = P.show

instance (Show a, Show b) => Show (a, b) where
  show' (a,b) = "(" ++ show' a ++ "," ++ show' b ++ ")"


-- Generic instances
instance (Show a) => Show (Maybe a) where show' = show


-- Utilities
space :: String -> String -> String
space a b = a ++ " " ++ b

paren :: String -> String
paren x = "(" ++ x ++ ")"

wrap :: a -> a -> [a] -> [a]
wrap h t l = h:l++[t]

{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where

import Prelude hiding (lookup)
import Char (ord)
import qualified Data.Map as Map
import Control.Monad ((>=>))
import Generics.Instant

-- Generalized tries, as from http://www.haskell.org/haskellwiki/GHC/Type_families#An_associated_data_type_example

class Representable k => GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v        = GMapInt (Map.Map Int v)
  empty                  = GMapInt Map.empty
  lookup k (GMapInt m)   = Map.lookup k m
  insert k v (GMapInt m) = GMapInt (Map.insert k v m)

instance GMapKey Char where
  data GMap Char v        = GMapChar (GMap Int v)
  empty                   = GMapChar empty
  lookup k (GMapChar m)   = lookup (ord k) m
  insert k v (GMapChar m) = GMapChar (insert (ord k) v m)

instance GMapKey U where
  data GMap U v           = GMapUnit (Maybe v)
  empty                   = GMapUnit Nothing
  lookup U (GMapUnit v)   = v
  insert U v (GMapUnit _) = GMapUnit $ Just v
  
instance (GMapKey a, GMapKey b) => GMapKey (a :*: b) where
  data GMap (a :*: b) v            = GMapProd (GMap a (GMap b v))
  empty                            = GMapProd empty
  lookup (a :*: b) (GMapProd gm)   = lookup a gm >>= lookup b 
  insert (a :*: b) v (GMapProd gm) = 
    GMapProd $ case lookup a gm of
      Nothing  -> insert a (insert b v empty) gm
      Just gm2 -> insert a (insert b v gm2  ) gm

instance (GMapKey a, GMapKey b) => GMapKey (a :+: b) where
  data GMap (a :+: b) v             = GMapSum (GMap a v) (GMap b v)
  empty                             = GMapSum empty empty
  lookup (L  a) (GMapSum gm1  _gm2) = lookup a gm1
  lookup (R b) (GMapSum _gm1 gm2 )  = lookup b gm2
  insert (L  a) v (GMapSum gm1 gm2) = GMapSum (insert a v gm1) gm2
  insert (R a) v (GMapSum gm1 gm2)  = GMapSum gm1 (insert a v gm2)

-- Uninteresting cases, but necessary
instance (GMapKey a) => GMapKey (C c a) where
  data GMap (C c a) v        = GMapCon (GMap a v)
  empty                      = GMapCon empty
  lookup (C c) (GMapCon m)   = lookup c m
  insert (C c) v (GMapCon m) = GMapCon (insert c v m)

instance (GMapKey a) => GMapKey (Var a) where
  data GMap (Var a) v          = GMapVar (GMap a v)
  empty                        = GMapVar empty
  lookup (Var x) (GMapVar m)   = lookup x m
  insert (Var x) v (GMapVar m) = GMapVar (insert x v m)

instance (GMapKey a) => GMapKey (Rec a) where
  data GMap (Rec a) v          = GMapRec (GMap a v)
  empty                        = GMapRec empty
  lookup (Rec x) (GMapRec m)   = lookup x m
  insert (Rec x) v (GMapRec m) = GMapRec (insert x v m)
  
-- Boilerplate code, but unavoidable (for now)
instance GMapKey k => GMapKey [k] where
  data GMap [k] v = GMapList (GMap (Rep [k]) v)
  
  empty = GMapList empty
  lookup k (GMapList m) = lookup (from k) m
  insert k v (GMapList m) = GMapList (insert (from k) v m)

-- Example
t1 :: Maybe String
t1 = lookup [1,2,3] $ insert ([1..3] :: [Int]) "[1,2,3]" $ empty

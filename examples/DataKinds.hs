{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Functions

--------------------------------------------------------------------------------
-- Indices with non-* kind, no existential quantification
--------------------------------------------------------------------------------

data I = I

data G1 :: I -> * where
  G11 :: Int   -> G1 'I
  G12 :: G1 'I -> G1 a

$(deriveAll ''G1)

-- Generic function instances
simplInstance ''GShow ''G1 'gshow 'gshowDefault
gadtInstance ''GEnum ''G1 'genum' 'genum

-- Testing
gshowG1 = gshow (G12 (G11 3))
genumG1 = gshow (take 50 $ genum :: [G1 'I])

--------------------------------------------------------------------------------
-- Indices with non-* kind, plus existential quantification
--------------------------------------------------------------------------------

-- data Nat = Ze | Su Nat

data Vec (a :: *) (n :: Nat) where
  Nil  :: Vec a 'Ze
  Cons :: a -> Vec a n -> Vec a ('Su n)

deriveAll ''Vec

-- Generic function instances
-- These are not automatically generated because of the parameter `a`
-- The user needs to supply the instance context
instance (GShow a) => GShow (Vec a n) where gshow = gshowDefault
instance (GEnum a, GEnum (Vec a n)) => GEnum (Vec a ('Su n)) where
  genum' = genum
instance (GEnum a) => GEnum (Vec a 'Ze) where genum' = genum


-- Testing
gshowVec = gshow (Cons 'p' Nil)
genumVec = gshow . take 10 $ (genum :: [Vec Int ('Su ('Su 'Ze))])

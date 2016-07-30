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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Functions

-------------------------------------------------------------------------------
-- Simple Datatype
-------------------------------------------------------------------------------

-- Example datatype
data Exp = Const Int | Plus Exp Exp

data Const
data Plus

instance Constructor Const where conName _ = "Const"
instance Constructor Plus  where conName _ = "Plus"

-- Representable instance
instance Representable Exp where
  type Rep Exp = C Const (Var Int) :+: C Plus (Rec Exp :*: Rec Exp)

  from (Const n)   = L (C (Var n))
  from (Plus e e') = R (C (Rec e :*: Rec e'))

  to (L (C (Var n)))            = Const n
  to (R (C (Rec e :*: Rec e'))) = Plus e e'

exp1 = Plus (Const 1) (Const 2)
exp2 = Plus exp1 (Const 3)

instance GEq Exp where geq = geqDefault

testExp1 :: (Bool, Bool)
testExp1 = (geq exp2 exp2, geq exp1 exp2)

instance Empty Exp where empty' = empty

testExp2 :: Exp
testExp2 = empty

instance GShow Exp where gshow = gshowDefault
instance Show Exp  where show = gshow -- convenience

testExp3 :: String
testExp3 = show exp2
{-
instance Update Exp where update' = update
instance MapOn Int where mapOn = (+1)

testExp4 :: Exp
testExp4 = update exp2
-}
-------------------------------------------------------------------------------
-- Mutually recursive datatypes
-------------------------------------------------------------------------------

data Decl = None | Seq Decl Decl | Assign String Expr

data Expr = V String | Lam String Expr | App Expr Expr | Let Decl Expr

-- Using TH
$(deriveAll ''Decl)
$(deriveAll ''Expr)


decls = Seq (Assign "x" (Lam "z" (V "z"))) (Assign "y" (V "x"))
expr  = Let decls (App (V "x") (V "y"))

instance GShow Expr where gshow = gshowDefault
instance GShow Decl where gshow = gshowDefault
instance Show Expr where show = gshow -- convenience
instance Show Decl where show = gshow -- convenience

testAST1 :: String
testAST1 = show expr

instance Empty Expr where empty' = empty
instance Empty Decl where empty' = empty

testAST2 :: Expr
testAST2 = empty

testAST3 :: Decl
testAST3 = empty

instance GEq Expr where geq = geqDefault
instance GEq Decl where geq = geqDefault

testAST4 :: Bool
testAST4 = geq expr expr
{-
instance Update Decl where update' = update
instance Update Expr where update' = update
instance MapOn [Char] where mapOn _ = "a"

testAST5 :: Decl
testAST5 = update decls

testAST6 :: Expr
testAST6 = update expr
-}

-------------------------------------------------------------------------------
-- Equality constraints
-------------------------------------------------------------------------------

-- Example 1

-- G1 has one index
data G1 :: * -> * where
  G11 :: Int    -> G1 Int
  G12 :: G1 Int -> G1 a

$(deriveAll ''G1)


-- Generic function instances
simplInstance ''GShow ''G1 'gshow 'gshowDefault
gadtInstance ''GEnum ''G1 'genum' 'genum

-- Testing
gshowG1 = gshow (G12 (G11 3))
genumG1 = gshow (take 100 $ genum :: [G1 Int])


-- Example 2: vectors

data Ze
data Su n

-- Vec has a parameter 'a' and an index 'n'
data Vec a :: * -> * where
  Nil :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

deriveAll ''Vec

-- Generic function instances
-- These are not automatically generated because of the parameter `a`
-- The user needs to supply the instance context
instance (GShow a) => GShow (Vec a n) where gshow = gshowDefault

instance (GEnum a, GEnum (Vec a n)) => GEnum (Vec a (Su n)) where
  genum' = genum

instance (GEnum a) => GEnum (Vec a Ze) where
  genum' = genum


-- Testing
gshowVec = gshow (Cons 'p' Nil)
genumVec = gshow . take 10 $ (genum :: [Vec Int (Su (Su Ze))])


-- Example 3: terms

-- Term has one index
data Term :: * -> * where
  Lit    :: Int -> Term Int
  IsZero :: Term Int -> Term Bool
  Pair   :: Term a -> Term b -> Term (a,b)
  If     :: Term Bool -> Term a -> Term a -> Term a

deriveAll ''Term

-- Generic function instances
simplInstance ''GShow ''Term 'gshow 'gshowDefault
gadtInstance ''GEnum ''Term 'genum' 'genum


-- Testing
gshowTerm = gshow (Pair (If (IsZero (Lit 1)) (Lit 2) (Lit 0)) (Lit 1))
genumTerm = gshow (take 10 (genum :: [Term (Bool,Int)]))

-- Example 4: Fin

data Fin n where
  FZe ::          Fin (Su n)
  FSu :: Fin n -> Fin (Su n)

deriveAll ''Fin

simplInstance ''GShow ''Fin 'gshow 'gshowDefault
gadtInstance  ''GEnum ''Fin 'genum' 'genum
-- We need to give this instance manually because the index Ze is never
-- used in the datatype definition
instance GEnum (Fin Ze) where genum' = []

gshowFin = gshow (FSu (FSu FZe))
genumFin = gshow (take 10 (genum :: [Fin (Su (Su Ze))]))

{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE EmptyDataDecls         #-}

import Generics.Instant
import Generics.Instant.Functions
import Prelude hiding (Eq, Show(..))

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

instance Eq Exp where eq' = eq

test1 :: (Bool, Bool)
test1 = (eq exp2 exp2, eq exp1 exp2)

instance Empty Exp where empty' = empty

test2 :: Exp
test2 = empty

instance Show Exp where show' = show

test3 :: String
test3 = show exp2

-------------------------------------------------------------------------------
-- Mutually recursive datatypes
-------------------------------------------------------------------------------

data Decl = None | Seq Decl Decl | Assign String Expr

data Expr = V String | Lam String Expr | App Expr Expr | Let Decl Expr

data None
data Seq
data Assign
data V
data Lam
data App
data Let

instance Constructor None   where conName _ = "None"
instance Constructor Seq    where conName _ = "Seq"
instance Constructor Assign where conName _ = "Assign"
instance Constructor V      where conName _ = "V"
instance Constructor Lam    where conName _ = "Lam"
instance Constructor App    where conName _ = "App"
instance Constructor Let    where conName _ = "Let"

instance Representable Decl where
  type Rep Decl =     C None U 
                 :+: C Seq (Rec Decl :*: Rec Decl)
                 :+: C Assign (Var String :*: Var Expr)

  from None         = L (C U)
  from (Seq d1 d2)  = R (L (C (Rec d1 :*: Rec d2)))
  from (Assign v e) = R (R (C (Var v :*: Var e)))

  to (L (C U))                       = None
  to (R (L (C (Rec d1 :*: Rec d2)))) = Seq d1 d2
  to (R (R (C (Var v :*: Var e))))   = Assign v e

instance Representable Expr where
  type Rep Expr =     C V (Var String)
                 :+: C Lam (Var String :*: Rec Expr)
                 :+: C App (Rec Expr :*: Rec Expr)
                 :+: C Let (Var Decl :*: Rec Expr)

  from (V x)     = L (C (Var x))
  from (Lam v e) = R (L (C (Var v :*: Rec e)))
  from (App f e) = R (R (L (C (Rec f :*: Rec e))))
  from (Let d e) = R (R (R (C (Var d :*: Rec e))))

  to (L (C (Var x)))                   = V x
  to (R (L (C (Var v :*: Rec e))))     = Lam v e
  to (R (R (L (C (Rec f :*: Rec e))))) = App f e
  to (R (R (R (C (Var d :*: Rec e))))) = Let d e

decls = Seq (Assign "x" (Lam "z" (V "z"))) (Assign "y" (V "x"))
expr  = Let decls (App (V "x") (V "y"))

instance Show Expr where show' = show
instance Show Decl where show' = show

test4 :: String
test4 = show expr

instance Empty Expr where empty' = empty
instance Empty Decl where empty' = empty

test5 :: Expr
test5 = empty

test6 :: Decl
test6 = empty

instance Eq Expr where eq' = eq
instance Eq Decl where eq' = eq

test7 = eq expr expr

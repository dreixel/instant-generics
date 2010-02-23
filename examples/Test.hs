{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE EmptyDataDecls         #-}

import Generics.Instant
import Generics.Instant.Functions
import Prelude hiding (Eq, Show(..))
import qualified Prelude as P (Show(..))

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

testExp1 :: (Bool, Bool)
testExp1 = (eq exp2 exp2, eq exp1 exp2)

instance Empty Exp where empty' = empty

testExp2 :: Exp
testExp2 = empty

instance Show Exp where show' = show
instance P.Show Exp where show = show -- convenience

testExp3 :: String
testExp3 = show exp2

instance Update Exp where update' = update
instance MapOn Int where mapOn = (+1)

testExp4 :: Exp
testExp4 = update exp2

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
  type Rep Decl =    C None U 
                 :+: C Seq (Rec Decl :*: Rec Decl)
                 :+: C Assign (Var String :*: Rec Expr)

  from None         = L (C U)
  from (Seq d1 d2)  = R (L (C (Rec d1 :*: Rec d2)))
  from (Assign v e) = R (R (C (Var v :*: Rec e)))

  to (L (C U))                       = None
  to (R (L (C (Rec d1 :*: Rec d2)))) = Seq d1 d2
  to (R (R (C (Var v :*: Rec e))))   = Assign v e

instance Representable Expr where
  type Rep Expr =    C V (Var String)
                 :+: C Lam (Var String :*: Rec Expr)
                 :+: C App (Rec Expr :*: Rec Expr)
                 :+: C Let (Rec Decl :*: Rec Expr)

  from (V x)     = L (C (Var x))
  from (Lam v e) = R (L (C (Var v :*: Rec e)))
  from (App f e) = R (R (L (C (Rec f :*: Rec e))))
  from (Let d e) = R (R (R (C (Rec d :*: Rec e))))

  to (L (C (Var x)))                   = V x
  to (R (L (C (Var v :*: Rec e))))     = Lam v e
  to (R (R (L (C (Rec f :*: Rec e))))) = App f e
  to (R (R (R (C (Rec d :*: Rec e))))) = Let d e

decls = Seq (Assign "x" (Lam "z" (V "z"))) (Assign "y" (V "x"))
expr  = Let decls (App (V "x") (V "y"))

instance Show Expr where show' = show
instance Show Decl where show' = show
instance P.Show Expr where show = show -- convenience
instance P.Show Decl where show = show -- convenience

testAST1 :: String
testAST1 = show expr

instance Empty Expr where empty' = empty
instance Empty Decl where empty' = empty

testAST2 :: Expr
testAST2 = empty

testAST3 :: Decl
testAST3 = empty

instance Eq Expr where eq' = eq
instance Eq Decl where eq' = eq

testAST4 :: Bool
testAST4 = eq expr expr

instance Update Decl where update' = update
instance Update Expr where update' = update
instance MapOn [Char] where mapOn _ = "a"

testAST5 :: Decl
testAST5 = update decls

testAST6 :: Expr
testAST6 = update expr

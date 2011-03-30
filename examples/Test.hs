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

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Functions
import Prelude hiding (Eq, Show(..))
import qualified Prelude as P (Show(..))

--------------------------------------------------------------------------------
-- Generic enum

class GEnum a where
  genum' :: [a]

instance GEnum U where
  genum' = [U]

instance (GEnum a) => GEnum (Rec a) where
  genum' = map Rec genum'

instance (GEnum a) => GEnum (Var a) where
  genum' = map Var genum'

instance (GEnum a) => GEnum (CEq c p p a) where genum' = map C genum'
instance (GEnum a) => GEnum (CEq c p q a) where genum' = []

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
{-
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
-}

-- Using TH instead
$(deriveAll ''Decl)
$(deriveAll ''Expr)


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

{-
data G11C
data G12C

instance Constructor G11C where conName _ = "G11"
instance Constructor G12C where conName _ = "G12"

instance Representable (G1 a) where
  type Rep (G1 a) =   C G11C a Int (Rec Int)
                  :+: C' G12C (Rec (G1 Int))
  
  from (G11 i) = L (C (Rec i))
  from (G12 x) = R (C (Rec x))
  to (L (C (Rec i))) = G11 i
  to (R (C (Rec x))) = G12 x
-}
-- Generic function instances (should be automatically generated)
instance Show (G1 a) where show' = show
gadtInstance ''GEnum ''G1 'genum' 'genum
-- instance GEnum (G1 Int) where genum' = genum

-- Testing
gshowG1 = show (G12 (G11 3))
genumG1 = show (take 100 $ genum :: [G1 Int])


-- Example 2: vectors

-- Type-level naturals
data Ze
data Su n

-- Vec has a parameter 'a' and an index 'n'
data Vec a :: * -> * where
  Nil :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)
{-
data Vec a n =           (n ~ Ze)   => Nil
             | forall m. (n ~ Su m) => Cons a (Vec a m)
-}
deriveAll ''Vec

{-
-- Utilities for existentials
type family X a

data Imp -- not exported!

-- mobility
type instance X (Su n) = n
-- null case
type instance X Ze = Imp

-- Representation
data NilC
data ConsC

instance Constructor NilC  where conName _ = "Nil"
instance Constructor ConsC where conName _ = "Cons"

instance Representable (Vec a n) where
  type Rep (Vec a n) =   
        C NilC  n Ze         U
    :+: C ConsC n (Su (X n)) (Var a :*: Rec (Vec a (X n)))
  from Nil = L (C U)
  from (Cons h t) = R (C (Var h :*: Rec t))

  to (L (C U)) = Nil
  to (R (C (Var h :*: Rec t))) = Cons h t
-}
-- Generic function instances (should be automatically generated)

instance (Show a) => Show (Vec a n) where show' = show

--gadtInstance ''GEnum ''Vec 'genum' 'genum

instance (GEnum a, GEnum (Vec a n)) => GEnum (Vec a (Su n)) where
  genum' = genum

instance (GEnum a) => GEnum (Vec a Ze) where
  genum' = genum

instance GEnum (Vec a Z) where genum' = error "never happens"

-- Testing
gshowVec = show (Cons 'p' Nil)
genumVec = show . take 10 $ (genum :: [Vec Int (Su Ze)])


-- Example 3: terms

-- Term has one index
data Term :: * -> * where
  Lit    :: Int -> Term Int
  IsZero :: Term Int -> Term Bool
  Pair   :: Term a -> Term b -> Term (a,b)
  If     :: Term Bool -> Term a -> Term a -> Term a
{-
data Term a =            a ~ Int    => Lit Int
            |            a ~ Bool   => IsZero (Term Int)
            |            ()         => If (Term Bool) (Term a) (Term a)
            | forall b c. a ~ (b,c) => Pair (Term b) (Term c)
-}
deriveAll ''Term

{-
-- Utilities for existentials
type family Y a
type family YY a

-- mobility
type instance Y (a,b) = a
type instance YY (a,b) = b

-- null cases
type instance Y Int  = Z
type instance Y Bool = Z
type instance YY Int  = Z
type instance YY Bool = Z

-- Representation
data LitC
data IsZeroC
data IfC
data PairC

instance Constructor LitC    where conName _ = "Lit"
instance Constructor IsZeroC where conName _ = "IsZero"
instance Constructor IfC     where conName _ = "If"
instance Constructor PairC   where conName _ = "Pair"

instance Representable (Term a) where
  type Rep (Term a) =
          (CEq LitC a Int (Rec Int)
      :+:  CEq IsZeroC a Bool (Rec (Term Int)))
    :+:   (CEq PairC a (Y a, YY a) (Rec (Term (Y a)) :*: Rec (Term (YY a)))
      :+:  CEq IfC () () (Rec (Term Bool) :*: Rec (Term a) :*: Rec (Term a)))

  from (Lit n) = L (L (C (Rec n)))
  from (IsZero n) = L (R (C (Rec n)))
  from (If b x y) = R (R (C (Rec b :*: Rec x :*: Rec y)))
  from (Pair a b) = R (L (C (Rec a :*: Rec b)))

  to (L (L (C (Rec n)))) = Lit n
  to (L (R (C (Rec n)))) = IsZero n
  to (R (R (C (Rec b :*: Rec x :*: Rec y)))) = If b x y
  to (R (L (C (Rec a :*: Rec b)))) = Pair a b
-}
-- Generic function instances
--instance Show (Term a) where show' = show
simplInstance ''Show ''Term 'show' 'show
gadtInstance ''GEnum ''Term 'genum' 'genum

{-
instance GEnum (Term Int) where genum' = genum
instance GEnum (Term Bool) where genum' = genum
instance (GEnum (Term a), GEnum (Term b)) => GEnum (Term (a,b)) where
  genum' = genum

instance GEnum (Term Z) where genum' = error "never happens"
-}
-- Testing
gshowTerm = show (Pair (If (IsZero (Lit 1)) (Lit 2) (Lit 0)) (Lit 1))
genumTerm = show (take 10 (genum :: [Term (Bool,Int)]))

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MagicHash            #-}

module Generics.Instant.GDiff 
  ( diff, diffLen, GDiff
  , SEq(..), shallowEqDef
  , Children(..), childrenDef
  , Ex(..)
  ) where

import Control.Arrow
import Data.Array

import Data.Typeable
import GHC.Prim


-- GP lib
import Generics.Instant

--------------------------------------------------------------------------------

ucast :: a -> b
ucast = unsafeCoerce#

--------------------------------------------------------------------------------

class Children' a where
  children' :: a -> [Ex]
  children' _ = []

instance Children' U

instance (Children' a, Children' b) => Children' (a :+: b) where
  children' (L a) = children' a
  children' (R a) = children' a

instance (Children' a, Children' b) => Children' (a :*: b) where
  children' (a :*: b) = children' a ++ children' b 

instance (Children' a) => Children' (CEq c p q a) where
  children' (C a) = children' a

instance (GDiff a) => Children' (Var a) where
  children' (Var a) = [Ex a]

instance (GDiff a) => Children' (Rec a) where
  children' (Rec a) = [Ex a]

-- | Gets all the immediate children of a term
class Children a where
  children :: a -> [Ex]
  children _ = []

instance Children Char
instance Children Int

instance (GDiff a) => Children [a] where
  children []    = []
  children (h:t) = [Ex h, Ex t]

childrenDef :: (Representable a, Children' (Rep a)) => a -> [Ex]
childrenDef x = children' (from x)

--------------------------------------------------------------------------------

-- Shallow equality
class SEq' a where
  shallowEq' :: a -> a -> Bool

instance (SEq' a, SEq' b) => SEq' (a :+: b) where
  shallowEq' (L x) (L x') = shallowEq' x x'
  shallowEq' (R x) (R x') = shallowEq' x x'
  shallowEq' _      _     = False
  
instance SEq' (CEq c p q a) where
  shallowEq' _ _ = True


-- | Shallow equality: compare the constructor name only
class SEq a where
  shallowEq :: a -> a -> Bool

instance SEq Char where
  shallowEq = (==)
instance SEq Int  where 
  shallowEq = (==)
instance SEq [a]  where
  shallowEq [] [] = True
  shallowEq (_:_) (_:_) = True
  shallowEq _ _ = False

shallowEqDef :: (Representable a, SEq' (Rep a)) => a -> a -> Bool
shallowEqDef x y = shallowEq' (from x) (from y)

--------------------------------------------------------------------------------

-- | Tying the recursive knot
class (Typeable a, SEq a, Children a) => GDiff a

instance GDiff Char
instance GDiff Int
instance (GDiff a) => GDiff [a]

-- | Existentials
data Ex where Ex :: (GDiff a) => !a -> Ex

instance Show Ex where
  -- should improve this
  show (Ex a) = show (typeOf a)

--------------------------------------------------------------------------------

-- | Edit actions
data Edit = Cpy !Ex | Del !Ex | Ins !Ex deriving Show

-- | Editscript
data EditScript = ES !Int# [Edit] deriving Show

editScriptLen :: EditScript -> Int
editScriptLen (ES _ l) = editScriptLen' l where
  editScriptLen' []          = 0
  editScriptLen' ((Cpy _):t) = editScriptLen' t
  editScriptLen' (_      :t) = 1 + editScriptLen' t

infixr 4 &
(&) :: EditScript -> EditScript -> EditScript
l@(ES m _) & r@(ES n _) = if m <=# n then l else r

--------------------------------------------------------------------------------
-- Memoization
type Table = Array (Int,Int) EditScript

gdiffm :: [Ex] -> [Ex] -> EditScript
gdiffm x y = table ! (length x, length y) where
  table :: Table
  table = 
    array ((0,0),(length x,length y))
      [ ((m,n),ES 0# []) | m <- [0..length x], n <- [0..length y]] //

    [ ((0,n), add (Ins (y !! (n-1))) (0,n-1))
    | n <- [1..length y] ] //

    [ ((n,0), add (Del (x !! (n-1))) (n-1,0))
    | n <- [1..length x] ] //

    [ ((m,n), gen m n) | m <- [1..length x], n <- [1.. length y] ]

  gen m n = case (x !! (m-1), y !! (n-1)) of
    (Ex x', Ex y') -> (if typeOf x' == typeOf y' && x' `shallowEq` (ucast y')
                        -- && length xs == length ys -- do we need this?
                       then (add (Cpy (Ex x')) (m-1,n-1)) & alt
                       else alt) where
      alt = add (Del (x !! (m-1))) (m-1,n) &
            add (Ins (y !! (n-1))) (m,n-1)

  add :: Edit -> (Int,Int) -> EditScript
  add e (a,b) = case table ! (a,b) of ES n l -> ES (n +# 1#) (e:l)

allChildren :: Ex -> [Ex]
allChildren (Ex a) = Ex a : concatMap allChildren (children a)
{-
-- I thought this would use less memory, but apparently it doesn't
allChildren (Ex x) = ux : concatMap allChildren xs
  where xs  = children x
        uxs = map (\(Ex y) -> Ex (undefined `asTypeOf` y)) xs
        ux  = Ex . fst . fromJust $ build x uxs
-}

--------------------------------------------------------------------------------

-- Top level functions
-- | Generic diff
diff :: (GDiff a) => a -> a -> EditScript
diff x y = gdiffm (reverse (allChildren (Ex x))) (reverse (allChildren (Ex y)))

-- | Edit distance
diffLen :: (GDiff a) => a -> a -> Float
diffLen x y = fromIntegral (editScriptLen (diff x y)) / 
                fromIntegral (length (allChildren (Ex x)))

{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -w           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.TH
-- Copyright   :  (c) 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains Template Haskell code that can be used to
-- automatically generate the boilerplate code for the generic deriving
-- library.
-----------------------------------------------------------------------------

-- Adapted from Generics.Deriving.TH
module Generics.Instant.TH (
    -- * Main generator
      deriveAll

    -- * Individual generators
    , deriveConstructors
    , deriveRepresentable
    , deriveRep

    -- * Utilities
    , simplInstance
    , genRepName, typeVariables, tyVarBndrToName
  ) where

import Generics.Instant.Base

import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..))

import Data.List (intercalate)
import Control.Monad
import Debug.Trace


-- | Given the names of a generic class, a type to instantiate, a function in
-- the class and the default implementation, generates the code for a basic
-- generic instance.
simplInstance :: Name -> Name -> Name -> Name -> Q [Dec]
simplInstance cl ty fn df = do
  i  <- reify (genRepName ty)
  i' <- reify ty
  let typ = return (foldl (\a -> AppT a . VarT . tyVarBndrToName) 
                              (ConT ty) (typeVariables i'))
  fmap (: []) $ instanceD (cxt []) (conT cl `appT` typ)
    [funD fn [clause [] (normalB (varE df)) []]]



-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Constructor' instances and the 'Representable' instance.
deriveAll :: Name -> Q [Dec]
deriveAll n =
  do a <- deriveConstructors n
     b <- deriveRepresentable n
     return (a ++ b)

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Constructor'.
deriveConstructors :: Name -> Q [Dec]
deriveConstructors = constrInstance

-- | Given the type and the name (as string) for the Representable type
-- synonym to derive, generate the 'Representable' instance.
deriveRepresentable :: Name -> Q [Dec]
deriveRepresentable n = do
    rep <- deriveRep n
    inst <- deriveInst n
    return $ rep ++ inst

-- | Derive only the 'Rep' type synonym. Not needed if 'deriveRepresentable'
-- is used.
deriveRep :: Name -> Q [Dec]
deriveRep n = do
  i <- reify n
  fmap (:[]) $ tySynD (genRepName n) (typeVariables i) (repType n)

deriveInst :: Name -> Q [Dec]
deriveInst t = do
  i <- reify t
  let typ q = return $ foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT q) 
                (typeVariables i)
      inlPrg = pragInlD t (inlineSpecPhase True False True 1)
  fcs <- mkFrom t 1 0 t
  tcs <- mkTo   t 1 0 t
  liftM (:[]) $
    instanceD (cxt [])
      (conT ''Representable `appT` typ t)
        [ tySynInstD ''Rep [typ t] (typ (genRepName t))
        , {- inlPrg, -} funD 'from fcs, funD 'to tcs]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n _ cs _) -> mkInstance n cs
    TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
    _ -> return []
  where
    mkInstance n cs = do
      ds <- mapM (mkConstrData n) cs
      is <- mapM (mkConstrInstance n) cs
      return $ ds ++ is

typeVariables :: Info -> [TyVarBndr]
typeVariables (TyConI (DataD    _ _ tv _ _)) = tv
typeVariables (TyConI (NewtypeD _ _ tv _ _)) = tv
typeVariables _                           = []

tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: [Name] -> Name
genName = mkName . (++"_") . intercalate "_" . map nameBase

genRepName :: Name -> Name
genRepName = mkName . (++"_") . ("Rep"  ++) . nameBase

mkConstrData :: Name -> Con -> Q Dec
mkConstrData dt (NormalC n _) =
  dataD (cxt []) (genName [dt, n]) [] [] [] 
mkConstrData dt r@(RecC _ _) =
  mkConstrData dt (stripRecordNames r)
mkConstrData dt (InfixC t1 n t2) =
  mkConstrData dt (NormalC n [t1,t2])
-- Contexts are ignored
mkConstrData dt (ForallC _ _ c) = mkConstrData dt c

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkConstrInstance :: Name -> Con -> Q Dec
-- Contexts are ignored
mkConstrInstance dt (ForallC _ _ c) = mkConstrInstance dt c
mkConstrInstance dt (NormalC n _) = mkConstrInstanceWith dt n []
mkConstrInstance dt (RecC    n _) = mkConstrInstanceWith dt n
      [ funD 'conIsRecord [clause [wildP] (normalB (conE 'True)) []]]
mkConstrInstance dt (InfixC t1 n t2) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n d) = Infix (convertDirection d) n
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative

mkConstrInstanceWith :: Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dt n extra = 
  instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
    (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

repType :: Name -> Q Type
repType n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  (foldBal' sum (error "Empty datatypes are not supported.")
                    (map (repConGADT (dt, map tyVarBndrToName vs)) cs))
                TyConI (NewtypeD _ dt vs c _) ->
                  repConGADT (dt, map tyVarBndrToName vs) c
                TyConI (TySynD t _ _) -> error "type synonym?" 
                _ -> error "unknown construct" 
      --appT b (conT $ mkName (nameBase n))
      b where
    sum :: Q Type -> Q Type -> Q Type
    sum a b = conT ''(:+:) `appT` a `appT` b


repConGADT :: (Name, [Name]) -> Con -> Q Type
repConGADT d (ForallC _vs ctx c) = repCon d c (f ctx) where
  f ((EqualP t1 t2):r) = case f r of
                       (t1s,t2s) -> ( ConT (mkName ":*:") `AppT` t1 `AppT` t1s
                                    , ConT (mkName ":*:") `AppT` t2 `AppT` t2s)
  f (_:r) = f r
  f []    = baseEqs
repConGADT d c = repCon d c baseEqs

flattenEqs :: (Type, Type) -> Q Type
flattenEqs (t1, t2) = return t1 `appT` return t2

-- () ~ ()
baseEqs :: (Type, Type)
baseEqs = (TupleT 0, TupleT 0)

repCon :: (Name, [Name]) -> Con -> (Type,Type) -> Q Type
repCon _ (ForallC _ _ _) _ = error "impossible"
repCon (dt, vs) (NormalC n []) (t1,t2) =
    conT ''CEq `appT` (conT $ genName [dt, n]) `appT` return t1 
                                               `appT` return t2 `appT` conT ''U
repCon (dt, vs) (NormalC n fs) (t1,t2) =
    conT ''CEq `appT` (conT $ genName [dt, n]) `appT` return t1 
                                               `appT` return t2 `appT` 
     (foldBal prod (map (repField (dt, vs) . snd) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
repCon (dt, vs) r@(RecC n []) (t1,t2)  =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` return t1
                                             `appT` return t2 `appT` conT ''U
repCon (dt, vs) r@(RecC n fs) (t1,t2) =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` return t1 
                                             `appT` return t2 `appT` 
      (foldBal prod (map (repField' (dt, vs) n) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
repCon d (InfixC t1 n t2) eqs = repCon d (NormalC n [t1,t2]) eqs

--dataDeclToType :: (Name, [Name]) -> Type
--dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

repField :: (Name, [Name]) -> Type -> Q Type
--repField d t | t == dataDeclToType d = conT ''I
repField d t = conT ''Rec `appT` return t

repField' :: (Name, [Name]) -> Name -> (Name, Strict, Type) -> Q Type
--repField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
repField' (dt, vs) ns (f, _, t) = conT ''Rec `appT` return t
-- Note: we should generate Var too, at some point


mkFrom :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = e -- lrE m i e
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (fromCon wrapE ns (dt, map tyVarBndrToName vs)
                    (length cs)) [1..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [fromCon wrapE ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [varP (field 0)] (normalB (wrapE $ conE 'K1 `appE` varE (field 0))) []]
                _ -> error "unknown construct"
      return b

mkTo :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkTo ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapP p = p -- lrP m i p
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (toCon wrapP ns (dt, map tyVarBndrToName vs)
                    (length cs)) [1..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [toCon wrapP ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [wrapP $ conP 'K1 [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct" 
      return b

fromCon :: (Q Exp -> Q Exp) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
-- Contexts are ignored
fromCon wrap ns d m i (ForallC _ _ c) = fromCon wrap ns d m i c
fromCon wrap ns (dt, vs) m i (NormalC cn []) =
  clause
    [conP cn []]
    (normalB $ wrap $ lrE m i $ appE (conE 'C) $ conE 'U) []
fromCon wrap ns (dt, vs) m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` 
      foldBal prod (zipWith (fromField (dt, vs)) [0..] (map snd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i r@(RecC cn []) =
  clause
    [conP cn []]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` (conE 'U)) []
fromCon wrap ns (dt, vs) m i r@(RecC cn fs) =
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` 
      foldBal prod (zipWith (fromField (dt, vs)) [0..] (map trd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  fromCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

fromField :: (Name, [Name]) -> Int -> Type -> Q Exp
--fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField (dt, vs) nr t = conE 'Rec `appE` varE (field nr)

toCon :: (Q Pat -> Q Pat) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
-- Contexts are ignored
toCon wrap ns d m i (ForallC _ _ c) = toCon wrap ns d m i c
toCon wrap ns (dt, vs) m i (NormalC cn []) =
    clause
      [wrap $ lrP m i $ conP 'C [conP 'U []]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ lrP m i $ conP 'C
        [foldBal prod (zipWith (toField (dt, vs)) [0..] (map snd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i r@(RecC cn []) =
    clause
      [wrap $ lrP m i $ conP 'U []]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i r@(RecC cn fs) =
    clause
      [wrap $ lrP m i $ conP 'C
        [foldBal prod (zipWith (toField (dt, vs)) [0..] (map trd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  toCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

toField :: (Name, [Name]) -> Int -> Type -> Q Pat
--toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField (dt, vs) nr t = conP 'Rec [varP (field nr)]


field :: Int -> Name
field n = mkName $ "f" ++ show n

lrP :: Int -> Int -> (Q Pat -> Q Pat)
{-
lrP 1 0 p = p
lrP m 0 p = conP 'L [p]
lrP m i p = conP 'R [lrP (m-1) (i-1) p]
-}
lrP m i p | m == 0       = error "1"
          | m == 1       = p
          | i <= div m 2 = conP 'L [lrP (div m 2)     i             p]
          | i >  div m 2 = conP 'R [lrP (m - div m 2) (i - div m 2) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
{-
lrE 1 0 e = e
lrE m 0 e = conE 'L `appE` e
lrE m i e = conE 'R `appE` lrE (m-1) (i-1) e
-}
lrE m i e | m == 0       = error "2"
          | m == 1       = e
          | i <= div m 2 = conE 'L `appE` lrE (div m 2)     i         e
          | i >  div m 2 = conE 'R `appE` lrE (m - div m 2) (i - div m 2) e

trd (_,_,c) = c

-- | Variant of foldr1 which returns a special element for empty lists
foldr1' f x [] = x
foldr1' _ _ [x] = x
foldr1' f x (h:t) = f h (foldr1' f x t)

-- | Variant of foldr1 for producing balanced lists
foldBal :: (a -> a -> a) -> [a] -> a
foldBal op = foldBal' op (error "foldBal: empty list")

foldBal' :: (a -> a -> a) -> a -> [a] -> a
foldBal' _  x []  = x
foldBal' _  _ [y] = y
foldBal' op x l   = let (a,b) = splitAt (length l `div` 2) l
                    in foldBal' op x a `op` foldBal' op x b

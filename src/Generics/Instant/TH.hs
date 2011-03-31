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
    , simplInstance, gadtInstance
    , genRepName, typeVariables, tyVarBndrToName
  ) where

import Generics.Instant.Base
import Generics.SYB (everywhere, mkT, everything, mkQ, gshow)

import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..), showName)

import Data.List (intercalate, nub, elemIndex)
import qualified Data.Map as M
import Control.Monad
import Control.Arrow ((&&&))
import Debug.Trace

-- Used by gadtInstance
data TypeArgsEqs = TypeArgsEqs { args :: [Type]        -- ^ Constructor args
                               , vars :: [Name]        -- ^ Variables
                               , teqs :: [(Type,Type)] -- ^ Type equalities
                               } deriving Show

-- | Given the names of a generic class, a type to instantiate, a function in
-- the class and the default implementation, generates the code for a basic
-- generic instance.
simplInstance :: Name -> Name -> Name -> Name -> Q [Dec]
simplInstance cl ty fn df = do
  i <- reify ty
  let typ = return (foldl (\a -> AppT a . VarT . tyVarBndrToName) 
                              (ConT ty) (typeVariables i))
  fmap (: []) $ instanceD (cxt []) (conT cl `appT` typ)
    [funD fn [clause [] (normalB (varE df)) []]]

-- | Given the names of a generic class, a GADT type to instantiate, a function
-- in the class and the default implementation, generates the code for a basic
-- generic instance. This is tricky in general because we have to analyze the
-- return types of each of the GADT constructors and give instances accordingly.
gadtInstance :: Name -> Name -> Name -> Name -> Q [Dec]
gadtInstance cl ty fn df = do
  i <- reify ty
  let typ = (foldl (\a -> AppT a . VarT . tyVarBndrToName) 
                              (ConT ty) (typeVariables i))

      dt :: ([TyVarBndr],[Con])
      dt = case i of
             TyConI (DataD _ _ vs cs _) -> (vs, cs)
             _ -> error ("gadtInstance: " ++ show ty ++ "is not a valid type")

      -- List of index variable names
      idxs :: [Name]
      idxs = extractIndices (fst dt) (snd dt)

      -- Get all the arguments, variables, and type equalities introduced by the
      -- constructors
      eqs :: [Name] -> [Con] -> [TypeArgsEqs]
      eqs nms cs = map f cs where
        f :: Con -> TypeArgsEqs
        f (NormalC _ tys)    = TypeArgsEqs (map snd tys)             [] []
        f (RecC _ tys)       = TypeArgsEqs (map (\(_,_,t) -> t) tys) [] []
        f (InfixC t1 _ t2)   = TypeArgsEqs [snd t1, snd t2]          [] []
        f (ForallC vs cxt c) = case f c of
            TypeArgsEqs ts vs' eqs' -> 
              TypeArgsEqs ts (tyVarBndrsToNames vs ++ vs') 
                          ((concatMap g cxt) ++ eqs')
        g :: Pred -> [(Type,Type)]
        g (EqualP (VarT t1) t2) | t1 `elem` nms = [(VarT t1,t2)]
                                | otherwise     = []
        g _                                     = []

      subst :: [(Type,Type)] -> Type -> Type
      subst s = everywhere (mkT f) where
        f (VarT a) = case lookup (VarT a) s of
                       Nothing -> VarT a
                       Just t  -> t
        f x        = x

      mkInst :: TypeArgsEqs -> Dec
      mkInst t = InstanceD (map mkCxt (args t)) 
                           (ConT cl `AppT` subst (teqs t) typ) instBody

      mkCxt :: Type -> Pred
      mkCxt = ClassP cl . (:[])

      -- The instance body is empty for regular cases
      instBody :: [Dec]
      instBody = [FunD fn [Clause [] (NormalB (VarE df)) []]]

      ncTys :: [(Type,Type)]
      ncTys = concatMap (nullCase []) (allCtxs (snd dt))
      allCtxs :: [Con] -> [Cxt]
      allCtxs = everything (++) ([] `mkQ` f) where
        f :: Con -> [Cxt]
        f (ForallC _ c _) = [c]
        f _               = []

      -- The instance body is undefined for the null cases
      mkNcInst :: Type -> Dec
      mkNcInst t = InstanceD [] (ConT cl `AppT` subst [(t,ConT ''Z)] typ) 
                     [FunD fn [Clause [] (NormalB errorE) []]]

      errorE :: Exp
--      errorE = VarE 'undefined
      errorE = VarE 'error `AppE` LitE (StringL $
                    "should not happen. Debug info:\ndt -> " ++ show dt
                 ++ "\nncTys -> " ++ show ncTys
                 ++ "\nallEqs -> " ++ show (eqs idxs (snd dt)))

      update :: Bool -> TypeArgsEqs -> [TypeArgsEqs] -> [TypeArgsEqs]
      update True  t1 [] = [t1]
      update False _  [] = []
      update b t1 (t2:ts) | teqs t1 == teqs t2 = 
                              t2 {args = nub (args t1 ++ args t2)} : ts
                          | otherwise          = t2 : update b t1 ts

      -- Types without any type equalities (not real GADTs) need to be handled
      -- differently. Others are dealt with using filterMerge.
      handleADTs :: ([TypeArgsEqs] -> [TypeArgsEqs]) 
                 -> [TypeArgsEqs] -> [TypeArgsEqs]
      handleADTs f ts | and (map (null . teqs) ts) 
                      = [TypeArgsEqs (concatMap args ts) [] []]
                      | otherwise = f ts                      

      -- We need to
      -- 1) ignore constructors that don't introduce any type equalities
      -- 2) merge constructors with the same return type
      -- This code is terribly inefficient and could easily be improved, btw.
      filterMerge :: [TypeArgsEqs] -> [TypeArgsEqs]
      filterMerge (t0@(TypeArgsEqs ts vs eqs):t)
        | eqs == [] = update True t0 (filterMerge t)
        | otherwise = case filterMerge t of
                        l -> if or (concat 
                                  [ [ typeMatch vs (vars t2) eq1 eq2
                                    | eq1 <- eqs, eq2 <- teqs t2 ] | t2 <- l ])
                             then update False t0 l
                             else t0 : l
      filterMerge [] = []

      -- For (2) above, we need to consider type equality modulo
      -- quantified-variable names
      typeMatch :: [Name] -> [Name] -> (Type,Type) -> (Type,Type) -> Bool
      typeMatch vs1 vs2 eq1 eq2 | length vs1 /= length vs2 = False 
                                | otherwise 
                                = eq1 == everywhere (mkT f) eq2
        where f (VarT n) = case n `elemIndex` vs2 of
                             -- is not a quantified variable
                             Nothing -> VarT n
                             -- it is, replace it with the equivalent var
                             Just i  -> VarT (vs1 !! i)
              f x        = x

      allTypeArgsEqs = eqs idxs (snd dt)
    
      normInsts = map mkInst   (handleADTs filterMerge allTypeArgsEqs)
      ncInsts   = map mkNcInst (nub (map fst ncTys))

  return $ normInsts ++ ncInsts


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

  let d = case i of
            TyConI dec -> dec
            _ -> error "unknown construct"
  
  exTyFams      <- genExTyFams d
  exTyFamsInsts <- genExTyFamInsts exTyFams d
  fmap (: (exTyFams ++ exTyFamsInsts)) $ 
    tySynD (genRepName n) (typeVariables i) (repType d (typeVariables i))

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

tyVarBndrsToNames :: [TyVarBndr] -> [Name]
tyVarBndrsToNames = map tyVarBndrToName

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

repType :: Dec -> [TyVarBndr] -> Q Type
repType i repVs = 
  do let sum :: Q Type -> Q Type -> Q Type
         sum a b = conT ''(:+:) `appT` a `appT` b
     case i of
        (DataD _ dt vs cs _)   ->
          (foldBal' sum (error "Empty datatypes are not supported.")
            (map (repConGADT (dt, tyVarBndrsToNames vs) repVs 
                   (extractIndices vs cs)) cs))
        (NewtypeD _ dt vs c _) -> repConGADT (dt, tyVarBndrsToNames vs) repVs
                                   (extractIndices vs [c]) c
        (TySynD t _ _)         -> error "type synonym?" 
        _                      -> error "unknown construct"


-- Given a datatype declaration, returns a list of its type variables which are
-- used as index and not as data
extractIndices :: [TyVarBndr] -> [Con] -> [Name]
extractIndices vs = nub . everything (++) ([] `mkQ` isIndexEq) where
  isIndexEq :: Pred -> [Name]
  isIndexEq (EqualP (VarT a) (VarT b)) = if a `elem` tyVarBndrsToNames vs
                                         then (a:)
                                           (if b `elem` tyVarBndrsToNames vs
                                           then [b] else []) else []
  isIndexEq (EqualP (VarT a) _)        = if a `elem` tyVarBndrsToNames vs
                                         then [a] else []
  isIndexEq (EqualP _ (VarT a))        = if a `elem` tyVarBndrsToNames vs
                                         then [a] else []
  isIndexEq _                          = []

repConGADT :: (Name, [Name]) -> [TyVarBndr] -> [Name] -> Con -> Q Type
-- We only accept one index variable, for now
repConGADT _ _ vs@(_:_:_) (ForallC _ _ _) = 
  error ("Datatype indexed over >1 variable: " ++ show vs)
-- Handle type equality constraints
repConGADT d@(dt, dtVs) repVs [indexVar] (ForallC vs ctx c) = 
  do
     let
        genTypeEqs ((EqualP t1 t2):r) | otherwise = case genTypeEqs r of 
            (t1s,t2s) -> ( ConT ''(:*:) `AppT` (substTyVar exEnv t1) `AppT` t1s
                         , ConT ''(:*:) `AppT` (substTyVar exEnv t2) `AppT` t2s)
        genTypeEqs (_:r) = genTypeEqs r -- other constraints are ignored
        genTypeEqs []    = baseEqs

        substTyVar :: M.Map Name Name -> Type -> Type
        substTyVar env = everywhere (mkT f) where
          f (VarT v) = case M.lookup v env of
                         Nothing -> VarT v
                         Just t  -> ConT t `AppT` VarT indexVar
          f x        = x

        exEnv :: M.Map Name Name
        exEnv = M.fromList . map (id &&& exTyFamName) $ tyVarBndrsToNames vs

     -- Go on with generating the representation type, taking the equalities
     repCon (dt, dtVs) (everywhere (mkT (substTyVar exEnv)) c) (genTypeEqs ctx)
-- No constraints, go on as usual
repConGADT d _repVs _ c = repCon d c baseEqs

-- Generates an existential type family name from a variable name
exTyFamName :: Name -> Name
exTyFamName = mkName . ("Ex_" ++) . showName

-- Generate a type family representing an existentially-quantified variable
genExTyFams :: Dec -> Q [Dec]
genExTyFams (DataD    _ _ _ cs _) = fmap concat (mapM genExTyFams' cs)
genExTyFams (NewtypeD _ _ _ c  _) = genExTyFams' c
genExTyFams _                     = return []

genExTyFams' :: Con -> Q [Dec]
genExTyFams' (ForallC vs _ _) =
  mapM (\x -> genExTyFams'' x (exTyFamName (tyVarBndrToName x))) vs
genExTyFams' _ = return []

genExTyFams'' :: TyVarBndr -> Name -> Q Dec
genExTyFams''   (PlainTV  n)   nm = genExTyFams'' (KindedTV n StarK) nm
genExTyFams'' b@(KindedTV n k) nm = familyKindD typeFam nm [b] StarK

-- Generate the mobility rules and null cases for the existential type families
genExTyFamInsts :: [Dec] -> Dec -> Q [Dec]
genExTyFamInsts ds (DataD    _ _ _ cs _) = fmap concat $ 
                                             mapM (genExTyFamInsts' ds) cs
genExTyFamInsts ds (NewtypeD _ _ _ c  _) = genExTyFamInsts' ds c

genExTyFamInsts' :: [Dec] -> Con -> Q [Dec]
genExTyFamInsts' decs (ForallC vs cxt c) = 
  do let nC = nullCase      (tyVarBndrsToNames vs) cxt
         mR = mobilityRules (tyVarBndrsToNames vs) cxt

         exTyFamNames = map getName decs

         getName :: Dec -> Name
         getName (FamilyD _ nm _ _) = nm
         getName _                  = error "getName: impossible"

         tySynInst nm ty x = TySynInstD nm [ty] x

     return (  [ tySynInst (exTyFamName nm) ty (VarT nm) | (nm, ty) <- mR ]
            ++ [ tySynInst nm ty (ConT ''Z) | (_,ty) <- nC, nm <- exTyFamNames])
genExTyFamInsts' _ _ = return []

-- Compute the types which need null cases for every existential type family
nullCase :: [a] -> Cxt -> [(Type,Type)]
nullCase [] = concatMap nullCase' where
  nullCase' :: Pred -> [(Type,Type)]
  nullCase' (EqualP (VarT _) (VarT _)) = []
  nullCase' (EqualP (VarT a) x) = if (everything (||) (False `mkQ` hasVar) x)
                                    then [] else [(VarT a, x)]
  nullCase' (EqualP x (VarT a)) = nullCase' (EqualP (VarT a) x)
  nullCase' _                   = []
  hasVar :: Type -> Bool
  hasVar (VarT _) = True
  hasVar _        = False
nullCase _ = const []

-- Compute the shape of the mobility rules
mobilityRules :: [Name] -> Cxt -> [(Name,Type)]
mobilityRules [] _   = []
mobilityRules vs cxt = concat [ mobilityRules' v p | v <- vs, p <- cxt ] where
  mobilityRules' :: Name -> Pred -> [(Name,Type)]
  mobilityRules' _ (EqualP (VarT _) (VarT _)) = []
  mobilityRules' v (EqualP (VarT a) x) | v `inComplex` x = [(v,x)]
                                       | otherwise       = []
  mobilityRules' v (EqualP x (VarT a)) = mobilityRules' v (EqualP (VarT a) x)
  mobilityRules' v _                   = []

  inComplex :: Name -> Type -> Bool
  inComplex v (VarT _) = False
  inComplex v x = everything (||) (False `mkQ` q) x where
    q (VarT x) | x == v    = True
    q (VarT x) | otherwise = False
    q _                    = False

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

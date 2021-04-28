{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Control.Env.Hierarchical.TH
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Control.Env.Hierarchical.TH (deriveEnv) where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1, Super, superL),
    Extends,
    Field (fieldL),
    Root,
    rootL,
  )
import Control.Monad (filterM, guard, zipWithM)
import Data.Function ((&))
import Language.Haskell.TH
  ( Dec (TySynD),
    DecQ,
    Info (TyConI),
    Inline (Inline),
    Name,
    Phases (AllPhases),
    Q,
    RuleMatch (FunLike),
    TyVarBndr,
    Type (AppT, ConT),
    TypeQ,
    appE,
    appT,
    clause,
    conE,
    conP,
    conT,
    cxt,
    funD,
    instanceD,
    lam1E,
    mkName,
    normalB,
    pprint,
    pragInlD,
    promotedConsT,
    promotedNilT,
    reify,
    reportError,
    reportWarning,
    tySynEqn,
    tySynInstD,
    valD,
    varE,
    varP,
  )
import qualified Language.Haskell.TH.Datatype as D
import Language.Haskell.TH.Ppr (commaSep)

deriveEnv :: Name -> Q [Dec]
deriveEnv envName = do
  envInfo <- D.reifyDatatype envName
  consInfo <- case D.datatypeCons envInfo of
    [consInfo] -> pure consInfo
    _ -> fail "Multiple costructors"
  let envType = D.datatypeType envInfo
      tyVars = D.datatypeVars envInfo
      fields = D.constructorFields consInfo
  dec <- envInstance (envType, consInfo, tyVars)
  decs <-
    zipWithM
      (deriveField (consInfo, envType))
      fields
      [0 ..]
  pure (dec : decs)

-- instance Field $ty $env where
--   $(deriveLens ...)
deriveField :: (D.ConstructorInfo, Type) -> Type -> Int -> Q Dec
deriveField (conInfo, envType) fieldType fieldIdx =
  instanceD (cxt []) fieldInstType [inlineDec, dec]
  where
    fieldInstType =
      conT ''Field `appT` pure fieldType `appT` pure envType
    -- fieldL = l where $(makeLensesFor ...)
    inlineDec = pragInlD 'fieldL Inline FunLike AllPhases
    dec = deriveLens conInfo 'fieldL fieldIdx

-- $lname f ($con x_1 ... x_n)= fmap (\y_$idx -> $con x_1 ... y_idx ... x_n) (f x_$idx)

deriveLens :: D.ConstructorInfo -> Name -> Int -> Q Dec
deriveLens conInfo lname idx = funD lname [clause argsP (normalB bodyE) []]
  where
    argsP = [varP f, conP conName conArgsP]
    conName = D.constructorName conInfo
    conArgsP = map varP args
    bodyE = varE 'fmap `appE` setterE `appE` appE (varE f) (varE x_idx)
    setterE = lam1E (varP y) (foldl appE (conE conName) argsE)
    argsE = [varE $ if i == idx then y else x | (x, i) <- zip args [0 ..]]
    f = mkName "f"
    y = mkName "y"
    x_idx = args !! idx
    args = [mkName ("x_" ++ show i) | i <- [1 .. arity]]
    arity = length $ D.constructorFields conInfo

envInstance :: (Type, D.ConstructorInfo, [TyVarBndr]) -> DecQ
envInstance (envType, consInfo, tyVars) =
  instanceD (cxt []) envInstType decs
  where
    -- instance Environment $envName where
    --   $decs
    envInstType = conT ''Environment `appT` envTypeQ
    envTypeQ = pure envType
    -- envType = D.datatypeType info
    decs :: [DecQ]
    decs = [fieldsDec, fields1Dec, superDec] ++ [superLDec | null extendsT]
    -- tyVars = D.datatypeVars info
    -- type Fields ($envName $typeVars) = '[$field1 ... $field2]
    fieldsDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Fields `appT` envTypeQ
        rhs = promotedListT (envType : D.constructorFields consInfo)
    -- type Fields1 ($envName $typeVars) = '[$obj1 ... $obj2]
    fields1Dec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Fields1 `appT` envTypeQ
        rhs = promotedListT =<< fields1 envType consInfo

    -- Super ($envName $typeVars) = $t
    -- where @$Extends $t@ is a field of the environment
    superDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Super `appT` envTypeQ
        rhs = case extendsT of
          [t] -> pure t
          [] -> conT ''Root
          ts@(t : _) -> do
            reportError $ "Multiple inheritance is not allowed: " <> show (commaSep ts)
            pure t
    extendsT :: [Type]
    extendsT = do
      AppT (ConT conName) t <- D.constructorFields consInfo
      guard $ conName == ''Extends
      pure t

    -- superL = rootL (only if @Super ($envName $typeVars) = Root@)
    superLDec = valD (varP 'superL) (normalB (varE 'rootL)) []

fields1 :: Type -> D.ConstructorInfo -> Q [Type]
fields1 ty consInfo =
  [f | AppT f x <- D.constructorFields consInfo, x == ty]
    & filterM headIsNotTypeSynonym
  where
    headIsNotTypeSynonym _ty = go _ty
      where
        go (AppT ty' _) = go ty'
        go (ConT name) = do
          r <- reify name
          case r of
            TyConI TySynD {} -> do
              reportWarning ("Skipping type synonym field1: " ++ pprint _ty ++ ". Please use newtype")
              pure False
            _ -> pure True
        go _ = pure True

promotedListT :: [Type] -> TypeQ
promotedListT =
  foldr (appT . appT promotedConsT . pure) promotedNilT

-- type Fields ($envName $typeVars) = $fields

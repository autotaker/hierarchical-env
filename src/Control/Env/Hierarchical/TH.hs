{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Env.Hierarchical.TH (deriveEnv) where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1),
    Field (fieldL),
  )
import Control.Monad (filterM, zipWithM)
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
    reportWarning,
    tySynEqn,
    tySynInstD,
    varE,
    varP,
  )
import qualified Language.Haskell.TH.Datatype as D

{-
data Env f a b = Env {
    _x :: Int,
    _y :: a,
    _z :: Obj Env,
    _w :: Obj2 b Env
    _v :: f Env

}
deriveEnvironment ''Env
==>
instance Environment (Env f a b) where
    type Fields (Env f a b) = '[Env f a b, Int, a,Obj Env, Obj2 b Env, f Env]
    type Fields1 (Env f a b) = '[Obj, Obj2 b, f]

instance Field Int (Env f a b) where
  fieldL = x

instance Field a (Env f a b) where
  fieldL = y

instance Field (Obj Env) (Env f a b) where
  fieldL = z

instance Field (Obj2 b Env) (Env f a b) where
  fieldL = w

instance Field (f Env) (Env f a b) where
  fieldL = v
-}

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
    decs = [fieldsDec, fields1Dec]
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

{-# LANGUAGE TemplateHaskell #-}

module Control.Env.Hierarchical.TH (deriveEnv) where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1, Super),
    Field (fieldL),
  )
import Control.Monad (filterM, zipWithM)
import Data.Function ((&))
import Language.Haskell.TH
import qualified Language.Haskell.TH.Datatype as D
import Lens.Micro.TH (makeLensesFor)

{-
data Env f a b = Env {
    _x :: Int,
    _y :: a,
    _z :: Obj Env,
    _w :: Obj2 b Env
    _v :: f Env

}
deriveEnvironment ''Env ''Root
==>
instance Environment (Env f a b) where
    type Super (Env f a b) = Root
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

deriveEnv :: Name -> Name -> Q [Dec]
deriveEnv envName rootName = do
  rootType <- conT rootName
  envInfo <- D.reifyDatatype envName
  consInfo <- case D.datatypeCons envInfo of
    [consInfo] -> pure consInfo
    _ -> fail "Multiple costructors"
  fieldNames <- case D.constructorVariant consInfo of
    D.RecordConstructor fieldNames -> pure fieldNames
    _ -> fail $ "Consturctor " <> pprint (D.constructorName consInfo) <> " is not a record"
  let envType = D.datatypeType envInfo
      tyVars = D.datatypeVars envInfo
      fields = D.constructorFields consInfo
  dec <- envInstance (envType, consInfo, tyVars) rootType
  decs <-
    zipWithM
      (deriveField (envName, envType))
      fields
      fieldNames
  pure (dec : decs)

-- instance Field $ty $env where
--   fieldL = l
--     where
--     $(makeLensesFor ["field1", "l"] ''Env)
deriveField :: (Name, Type) -> Type -> Name -> Q Dec
deriveField (envName, envType) fieldType fieldName =
  instanceD (cxt []) fieldInstType [inlineDec, dec]
  where
    fieldInstType =
      conT ''Field `appT` pure fieldType `appT` pure envType
    -- fieldL = l where $(makeLensesFor ...)
    inlineDec = pragInlD 'fieldL Inline FunLike AllPhases
    dec = do
      lensDecs <- makeLensesFor [(nameBase fieldName, "l")] envName
      let lhs = varP 'fieldL
          rhs = varE (mkName "l")
      valD lhs (normalB rhs) (map pure lensDecs)

envInstance :: (Type, D.ConstructorInfo, [TyVarBndr]) -> Type -> DecQ
envInstance (envType, consInfo, tyVars) rootType =
  instanceD (cxt []) envInstType decs
  where
    -- instance Environment $envName where
    --   $decs
    envInstType =
      conT ''Environment
        `appT` envTypeQ
    envTypeQ = pure envType
    -- envType = D.datatypeType info
    decs :: [DecQ]
    decs = [superDec, fieldsDec, fields1Dec]
    -- tyVars = D.datatypeVars info
    -- type Super ($envName $typeVars) = $rootType
    superDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Super `appT` envTypeQ
        rhs = pure rootType
    -- type Fields ($envName $typeVars) = '[]
    fieldsDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Fields `appT` envTypeQ
        rhs = promotedListT (envType : D.constructorFields consInfo)
    -- type Fields1 ($envName $typeVars) = '[]
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

{-# LANGUAGE TemplateHaskell #-}

module Control.Env.Hierarchical.TH where

import Control.Applicative (Alternative (empty))
import Control.Env.Hierarchical.Internal
import Control.Monad (filterM)
import Data.Function
import Language.Haskell.TH
import qualified Language.Haskell.TH.Datatype as D
import Lens.Micro.TH

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
    type Fields (Env f a b) = '[Int, a,Obj Env, Obj2 b Env, f Env]
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
  dec <- envInstance envInfo rootType
  pure [dec]

envInstance :: D.DatatypeInfo -> Type -> Q Dec
envInstance info rootType = do
  -- instance Environment $envName where
  --   $decs
  consInfo <- case D.datatypeCons info of
    [consInfo] -> pure consInfo
    _ -> fail "Multiple costructors"
  let envInstType =
        conT ''Environment
          `appT` envTypeQ
      envTypeQ = pure envType
      envType = D.datatypeType info
      decs :: [DecQ]
      decs = [superDec, fieldsDec, fields1Dec]
      tyVars = D.datatypeVars info
      -- type Super ($envName $typeVars) = $rootType
      superDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
        where
          lhs = conT ''Super `appT` envTypeQ
          rhs = pure rootType
      -- type Fields ($envName $typeVars) = '[]
      fieldsDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
        where
          lhs = conT ''Fields `appT` envTypeQ
          rhs = promotedListT (envType : fields consInfo)
      -- type Fields1 ($envName $typeVars) = '[]
      fields1Dec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
        where
          lhs = conT ''Fields1 `appT` envTypeQ
          rhs = promotedListT =<< fields1 envType consInfo
  instanceD (cxt []) envInstType decs

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

fields :: D.ConstructorInfo -> [Type]
fields = D.constructorFields

-- type Fields ($envName $typeVars) = $fields

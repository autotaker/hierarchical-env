{-# LANGUAGE TemplateHaskell #-}

module Control.Env.Hierarchical.TH where

import Control.Env.Hierarchical.Internal
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
envInstance info rootType =
  -- instance Environment $envName where
  --   $decs
  instanceD (cxt []) envInstType decs
  where
    envInstType =
      conT ''Environment
        `appT` envType
    envType = pure $ D.datatypeType info
    decs :: [DecQ]
    decs = [superDec, fieldsDec, fields1Dec]
    tyVars = D.datatypeVars info
    -- type Super ($envName $typeVars) = $rootType
    superDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Super `appT` envType
        rhs = pure rootType
    -- type Fields ($envName $typeVars) = '[]
    fieldsDec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Fields `appT` envType
        rhs = promotedNilT
    -- type Fields1 ($envName $typeVars) = '[]
    fields1Dec = tySynInstD (tySynEqn (Just tyVars) lhs rhs)
      where
        lhs = conT ''Fields1 `appT` envType
        rhs = promotedNilT

-- type Fields ($envName $typeVars) = $fields

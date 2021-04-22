{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module : Control.Env.Hierarchical.Internal
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Control.Env.Hierarchical.Internal
  ( Environment (..),
    Super,
    Root (..),
    Field (..),
    Trans (..),
    type (<:),
    SomeInterface (SomeInterface),
    Has,
    Has1,
    getL,
    ifaceL,
    runIF,
  )
where

import Data.Kind (Type)
import Data.Type.Bool (If)
import GHC.TypeLits
  ( ErrorMessage (ShowType, Text, (:<>:)),
    TypeError,
  )
import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)
import RIO (RIO, runRIO)

-- | @Super env@ represents the inheritance relation between environments.
-- Every environment must be a descendant of 'Root'.
type family Super env

class Environment env where
  -- | interfaces that are fields of the environment
  type Fields1 env :: [Type -> Type]

  -- | fields of the environment
  type Fields env :: [Type]

-- | Root environment that does not have any fields.
data Root = Root

type instance Super Root = TypeError ('Text "No super environment for Root")

instance Environment Root where
  type Fields1 Root = '[]
  type Fields Root = '[]

-- | direct field of @env@
class Field a env where
  fieldL :: Lens' env a

instance Field env env where
  fieldL = id

class Trans a (l :: [Type]) s | a l -> s where
  transL :: Lens' s a

instance Trans a '[] a where
  transL = id

instance (Field s' s, Trans a l s') => Trans a (s : l) s where
  transL = fieldL . transL @a @l

type family Addr a b :: [Type] where
  Addr a a = '[]
  Addr a b = a : Addr (Super a) b

type family Member (f :: k) (l :: [k]) :: Bool where
  Member f '[] = 'False
  Member f (f : l) = 'True
  Member f (g : l) = Member f l

type family FindEnv (f :: Type) (envs :: [Type]) where
  FindEnv f (env ': envs) = If (Member f (Fields env)) env (FindEnv f envs)
  FindEnv f '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

type family FindEnv1 (f :: Type -> Type) (envs :: [Type]) where
  FindEnv1 f (env ': envs) = If (Member f (Fields1 env)) env (FindEnv1 f envs)
  FindEnv1 f '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

type (<:) env env' = Trans env' (Addr env env') env

data SomeInterface f env where
  SomeInterface :: Lens' env' (f env') -> Lens' env env' -> SomeInterface f env

type HasAux a env env' = (env <: env', Field a env')

type Has1Aux f env env' = (env <: env', Field (f env') env')

-- | Type constraint meaning @env@ contains @a@ as a (including ancestors') field.
--
-- An environment @env@ contains unique value for each type @T@ that satisfies
-- @Has T env@. If you want to depends on multiple values of the same type,
-- please distinguish them by using newtype.
type family Has a env where
  Has a env = HasAux a env (FindEnv a (Ancestors env))

-- | Type constraint meaning @env@ contains @f env'@ for some ancestor @env'@
type family Has1 f env where
  Has1 f env = Has1Aux f env (FindEnv1 f (Ancestors env))

type Ancestors env = Addr env Root

inheritL :: forall env env'. env <: env' => Lens' env env'
inheritL = transL @env' @(Addr env env')

-- | Lens to extract @a@ from @env@
getL :: forall a env. Has a env => Lens' env a
getL = inheritL @env @(FindEnv a (Ancestors env)) . fieldL

ifaceL :: forall f env. Has1 f env => SomeInterface f env
ifaceL = SomeInterface (fieldL @(f (FindEnv1 f (Ancestors env)))) inheritL

-- | Run action that depends on an interface @f@.
-- The action must be polymorphic to @env'@,
-- because it will run in some ancestor environment, which may be different from @env@,
runIF :: forall f env a. Has1 f env => (forall env'. f env' -> RIO env' a) -> RIO env a
runIF body =
  case ifaceL @f of
    SomeInterface _ifaceL superL -> do
      iface <- view $ superL . _ifaceL
      env <- view superL
      runRIO env (body iface)
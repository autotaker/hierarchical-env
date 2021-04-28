{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
    Extends (..),
    Root (..),
    Field (..),
    Trans (..),
    SomeInterface (SomeInterface),
    Has,
    Has1,
    getL,
    ifaceL,
    runIF,
    rootL,
    extendsL,
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

class Environment env where
  -- | @Super env@ represents the inheritance relation between environments.
  --
  -- * If @env@ owns a field of the form @Extends T@, then @T@ is the super environment.
  -- * If @env@ owns no field of the form @Extends T@, then 'Root' is the super environment.
  -- * Every @env@ must have at most one field of the form @Extends T@ because multiple inheritance is not supported.
  type Super env

  -- | interfaces that are fields of the environment
  type Fields1 env :: [Type -> Type]

  -- | fields of the environment
  type Fields env :: [Type]

  -- | Lens to super environment
  superL :: Lens' env (Super env)
  {-# INLINE superL #-}
  default superL :: Field (Extends (Super env)) env => Lens' env (Super env)
  superL = fieldL . extendsL

-- | Root environment that does not have any fields.
data Root = Root

-- | Wrapper that represents the super environment.
newtype Extends env = Extends env

{-# INLINE extendsL #-}
extendsL :: Lens' (Extends x) x
extendsL f (Extends x) = fmap Extends (f x)

rootL :: Lens' x Root
rootL f x = x <$ f Root

instance Environment Root where
  type Super Root = TypeError ('Text "No super environment for Root")
  type Fields1 Root = '[]
  type Fields Root = '[]
  superL = undefined

-- | direct field of @env@
class Field a env where
  fieldL :: Lens' env a

instance Field env env where
  fieldL = id

-- Addr s t = '[v1, ... vn]
-- s = v1 <: v2 <: ... <: vn = t
-- s = env1 <: env2 <: env3 = Target s '[env2, env3]
-- s = env1 <: env2 = Target s '[env2]
-- s = env1 = Target s '[]
-- Trans env1 [env1]
class Trans s (l :: [Type]) where
  type Target s l
  transL :: Lens' s (Target s l)

instance Trans s '[] where
  type Target s '[] = s
  transL = id

instance (Environment s, Super s ~ t, Trans t l) => Trans s (t : l) where
  type Target s (t : l) = Target t l
  transL = superL . transL @t @l

-- env1 <: env2 <: env3
-- Addr env1 env3 = [env2, env3]
-- Addr env2 env3 = [env3]
-- Addr env3 env3 = []
type family Addr a :: [Type] where
  Addr Root = '[]
  Addr a = Super a ': Addr (Super a)

type family Member (f :: k) (l :: [k]) :: Bool where
  Member f '[] = 'False
  Member f (f : l) = 'True
  Member f (g : l) = Member f l

-- X <- Env1, FindEnv X Env1 [Env2,Root] => []
-- X <- Env2, FindEnv X Env1 [Env2,Root] => [Env2]
type family FindEnv (f :: Type) env (envs :: [Type]) :: [Type] where
  FindEnv f env (env' ': envs) = If (Member f (Fields env)) '[] (env' : FindEnv f env' envs)
  FindEnv f env '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

type family FindEnv1 (f :: Type -> Type) env (envs :: [Type]) :: [Type] where
  FindEnv1 f env (env' ': envs) = If (Member f (Fields1 env)) '[] (env' : FindEnv1 f env' envs)
  FindEnv1 f env '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

data SomeInterface f env where
  SomeInterface :: Lens' env' (f env') -> Lens' env env' -> SomeInterface f env

type HasAux a env route = (Trans env route, Field a (Target env route))

type Has1Aux f env route = (Trans env route, Field (f (Target env route)) (Target env route))

-- | Type constraint meaning @env@ contains @a@ as a (including ancestors') field.
--
-- An environment @env@ contains unique value for each type @T@ that satisfies
-- @Has T env@. If you want to depends on multiple values of the same type,
-- please distinguish them by using newtype.
type family Has a env where
  Has a env = HasAux a env (FindEnv a env (Addr env))

-- | Type constraint meaning @env@ contains @f env'@ for some ancestor @env'@
type family Has1 f env where
  Has1 f env = Has1Aux f env (FindEnv1 f env (Addr env))

-- | Lens to extract @a@ from @env@
getL :: forall a env. Has a env => Lens' env a
getL = transL @env @(FindEnv a env (Addr env)) . fieldL

ifaceL :: forall f env. Has1 f env => SomeInterface f env
ifaceL =
  SomeInterface
    (fieldL @(f (Target env (FindEnv1 f env (Addr env)))))
    (transL @env @(FindEnv1 f env (Addr env)))

-- | Run action that depends on an interface @f@.
-- The action must be polymorphic to @env'@,
-- because it will run in some ancestor environment, which may be different from @env@,
runIF :: forall f env a. Has1 f env => (forall env'. f env' -> RIO env' a) -> RIO env a
runIF body =
  case ifaceL @f of
    SomeInterface _ifaceL _superL -> do
      iface <- view $ _superL . _ifaceL
      env <- view _superL
      runRIO env (body iface)

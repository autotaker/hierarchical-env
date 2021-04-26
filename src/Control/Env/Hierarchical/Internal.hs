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
    --    type (<:),
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

instance (Field t s, Trans t l) => Trans s (t : l) where
  type Target s (t : l) = Target t l
  transL = fieldL . transL @t @l

-- env1 <: env2 <: env3
-- Addr env1 env3 = [env2, env3]
-- Addr env2 env3 = [env3]
-- Addr env3 env3 = []
type family Addr a :: [Type] where
  Addr Root = '[]
  Addr a = Super a ': Addr (Super a)

type family Ancestors env :: [Type] where
  Ancestors env = env ': Addr env

type family Member (f :: k) (l :: [k]) :: Bool where
  Member f '[] = 'False
  Member f (f : l) = 'True
  Member f (g : l) = Member f l

type family FindEnv (f :: Type) (envs :: [Type]) :: [Type] where
  FindEnv f (env ': envs) = env ': If (Member f (Fields env)) '[] (FindEnv f envs)
  FindEnv f '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

type family FindEnv1 (f :: Type -> Type) (envs :: [Type]) :: [Type] where
  FindEnv1 f (env ': envs) = env ': If (Member f (Fields1 env)) '[] (FindEnv1 f envs)
  FindEnv1 f '[] = TypeError ('Text "No environment has " ':<>: 'ShowType f)

-- type (<:) env env' = (Trans env (Tail (Addr env env')), Target env (Tail (Addr env env')) ~ env')

type family Tail (l :: [Type]) where
  Tail (x : xs) = xs

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
  Has a env = HasAux a env (Tail (FindEnv a (Ancestors env)))

-- | Type constraint meaning @env@ contains @f env'@ for some ancestor @env'@
type family Has1 f env where
  Has1 f env = Has1Aux f env (Tail (FindEnv1 f (Ancestors env)))

-- | Lens to extract @a@ from @env@
getL :: forall a env. Has a env => Lens' env a
getL = transL @env @(Tail (FindEnv a (Ancestors env))) . fieldL

ifaceL :: forall f env. Has1 f env => SomeInterface f env
ifaceL =
  SomeInterface
    (fieldL @(f (Target env (Tail (FindEnv1 f (Ancestors env))))))
    (transL @env @(Tail (FindEnv1 f (Ancestors env))))

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

{-
data Env1 = Env1 Int Char

type instance Super Env1 = Root

instance Environment Env1 where
  type Fields Env1 = '[Int, Char]
  type Fields1 Env1 = '[]

instance Field Int Env1 where
  fieldL f (Env1 x1 x2) = fmap (\y1 -> Env1 y1 x2) (f x1)

ex :: Lens' Env1 Int
ex = getL

ex2 :: Int
ex2 = t1 + t2
  where
    t1 :: (Ancestors Env1 ~ '[Env1, Root]) => Int
    t1 = 0
    t2 :: (FindEnv Int '[Env1, Root] ~ '[Env1]) => Int
    t2 = 0
    t3 :: Trans Env1 '[Env1] => Lens' Env1 Env1
    t3 = transL @Env1 @'[Env1]
    t4 :: Lens' Env1 Env1
    t4 = t3

-}
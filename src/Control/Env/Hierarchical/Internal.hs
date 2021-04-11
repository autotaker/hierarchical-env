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

module Control.Env.Hierarchical.Internal
  ( Environment (..),
    Root (..),
    Field (..),
    Trans (..),
    type (<:),
    Obj (Obj),
    Has,
    Has1,
    getL,
    getObj,
    runObj,
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
  type Super env
  type Fields1 env :: [Type -> Type]
  type Fields env :: [Type]

data Root = Root

instance Environment Root where
  type Super Root = TypeError ('Text "No super environment for Root")
  type Fields1 Root = '[]
  type Fields Root = '[]

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

data Obj f env where
  Obj :: Lens' env' (f env') -> Lens' env env' -> Obj f env

type HasAux a env env' = (env <: env', Field a env')

type Has1Aux f env env' = (env <: env', Field (f env') env')

type Has a env = HasAux a env (FindEnv a (Anscestors env))

type Has1 f env = Has1Aux f env (FindEnv1 f (Anscestors env))

type Anscestors env = Addr env Root

inheritL :: forall env env'. env <: env' => Lens' env env'
inheritL = transL @env' @(Addr env env')

getL :: forall a env. Has a env => Lens' env a
getL = inheritL @env @(FindEnv a (Anscestors env)) . fieldL

getObj :: forall f env. Has1 f env => Obj f env
getObj = Obj (fieldL @(f (FindEnv1 f (Anscestors env)))) inheritL

runObj :: Obj f env -> (forall env'. f env' -> RIO env' a) -> RIO env a
runObj (Obj objL superL) body = do
  obj <- view $ superL . objL
  env <- view superL
  runRIO env (body obj)

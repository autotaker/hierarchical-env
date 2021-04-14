{-# LANGUAGE ExplicitNamespaces #-}

module Control.Env.Hierarchical
  ( Environment,
    Root (..),
    Field (..),
    type (<:),
    Obj,
    Has,
    Has1,
    getL,
    getObj,
    runObj,
    deriveEnv,
  )
where

import Control.Env.Hierarchical.Internal
  ( Environment,
    Field (fieldL),
    Has,
    Has1,
    Obj,
    Root (Root),
    getL,
    getObj,
    runObj,
    type (<:),
  )
import Control.Env.Hierarchical.TH (deriveEnv)
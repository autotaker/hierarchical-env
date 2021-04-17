{-# LANGUAGE ExplicitNamespaces #-}

module Control.Env.Hierarchical
  ( Environment,
    Root (..),
    Field (..),
    type (<:),
    Has,
    Has1,
    getL,
    runIF,
    deriveEnv,
  )
where

import Control.Env.Hierarchical.Internal
  ( Environment,
    Field (fieldL),
    Has,
    Has1,
    Root (Root),
    getL,
    runIF,
    type (<:),
  )
import Control.Env.Hierarchical.TH (deriveEnv)
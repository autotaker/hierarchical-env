{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial3.Env where

import Control.Env.Hierarchical (deriveEnv)
import Tutorial3.App (appImpl)
import Tutorial3.Interface
  ( App,
    ConnectionPool,
    SlackWebhookURL,
  )

data Env = Env ConnectionPool SlackWebhookURL (App Env)

deriveEnv ''Env

mkEnv :: ConnectionPool -> SlackWebhookURL -> Env
mkEnv pool hook = Env pool hook appImpl

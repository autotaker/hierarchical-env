{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial3.Env where

import Control.Env.Hierarchical (deriveEnv, getL)
import RIO (HasLogFunc (logFuncL), LogFunc)
import Tutorial3.App (appImpl)
import Tutorial3.Interface
  ( App,
    ConnectionPool,
    SlackWebhookURL,
  )

data Env = Env
  { logFunc :: LogFunc,
    connectionPool :: ConnectionPool,
    slackWebhookURL :: SlackWebhookURL,
    app :: App Env
  }

deriveEnv ''Env

mkEnv :: LogFunc -> ConnectionPool -> SlackWebhookURL -> Env
mkEnv lf pool hook = Env lf pool hook appImpl

instance HasLogFunc Env where
  logFuncL = getL

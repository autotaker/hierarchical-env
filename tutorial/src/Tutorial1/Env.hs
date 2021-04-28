{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial1.Env where

import Control.Env.Hierarchical (deriveEnv, getL)
import RIO (HasLogFunc (logFuncL), LogFunc)
import Tutorial1.Interface (ConnectionPool, SlackWebhookURL)

data Env = Env
  { logFunc :: LogFunc,
    connectionPool :: ConnectionPool,
    slackWebhookURL :: SlackWebhookURL
  }

deriveEnv ''Env

instance HasLogFunc Env where
  logFuncL = getL

mkEnv :: LogFunc -> ConnectionPool -> SlackWebhookURL -> Env
mkEnv = Env

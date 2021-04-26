{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial3.Env where

import Control.Env.Hierarchical (deriveEnv, getL)
import RIO (HasLogFunc (logFuncL), LogFunc)
import Tutorial3.App (inqueryRepoImpl, slackAPIImpl)
import Tutorial3.Interface
  ( ConnectionPool,
    InqueryRepo,
    SlackAPI,
    SlackWebhookURL,
  )

data Env = Env
  { logFunc :: LogFunc,
    connectionPool :: ConnectionPool,
    slackWebhookURL :: SlackWebhookURL,
    inqueryRepo :: InqueryRepo Env,
    slackAPI :: SlackAPI Env
  }

deriveEnv ''Env

mkEnv :: LogFunc -> ConnectionPool -> SlackWebhookURL -> Env
mkEnv lf pool hook = Env lf pool hook inqueryRepoImpl slackAPIImpl

instance HasLogFunc Env where
  logFuncL = getL

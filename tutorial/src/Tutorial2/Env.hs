{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial2.Env where

import Control.Env.Hierarchical (deriveEnv, getL)
import RIO (HasLogFunc (logFuncL), LogFunc)
import Tutorial2.App (inqueryRepoImpl, slackAPIImpl)
import Tutorial2.Interface
  ( ConnectionPool,
    InqueryRepo,
    SlackAPI,
    SlackWebhookURL,
  )

data Env
  = Env
      LogFunc
      ConnectionPool
      SlackWebhookURL
      (InqueryRepo Env)
      (SlackAPI Env)

deriveEnv ''Env

mkEnv :: LogFunc -> ConnectionPool -> SlackWebhookURL -> Env
mkEnv lf pool hook = Env lf pool hook inqueryRepoImpl slackAPIImpl

instance HasLogFunc Env where
  logFuncL = getL

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial2.Env where

import Control.Env.Hierarchical (deriveEnv)
import Tutorial2.App (inqueryRepoImpl, slackAPIImpl)
import Tutorial2.Interface
  ( ConnectionPool,
    InqueryRepo,
    SlackAPI,
    SlackWebhookURL,
  )

data Env
  = Env
      ConnectionPool
      SlackWebhookURL
      (InqueryRepo Env)
      (SlackAPI Env)

deriveEnv ''Env

mkEnv :: ConnectionPool -> SlackWebhookURL -> Env
mkEnv pool hook = Env pool hook inqueryRepoImpl slackAPIImpl

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial1.Env where

import Control.Env.Hierarchical (deriveEnv)
import Tutorial1.Interface (ConnectionPool, SlackWebhookURL)

data Env = Env ConnectionPool SlackWebhookURL

deriveEnv ''Env

mkEnv :: ConnectionPool -> SlackWebhookURL -> Env
mkEnv = Env

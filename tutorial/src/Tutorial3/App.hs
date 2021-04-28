{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tutorial3.App where

import Control.Env.Hierarchical (deriveEnv, mapBaseRIO)
import Control.Env.Hierarchical.Internal
  ( Environment (superL),
    Extends (Extends),
    Has,
    Has1,
    getL,
    runIF,
  )
import Data.Aeson (KeyValue ((.=)), object)
import Data.Pool (withResource)
import Database.MySQL.Simple (Only (Only), query_)
import Network.HTTP.Simple
  ( httpNoBody,
    parseRequestThrow,
    setRequestBodyJSON,
  )
import RIO
  ( Display (display, textDisplay),
    HasLogFunc (logFuncL),
    MonadIO (liftIO),
    RIO,
    Text,
    displayShow,
    logInfo,
    view,
    void,
    (<&>),
  )
import Tutorial3.Interface
  ( App (App),
    ConnectionPool (ConnectionPool),
    InqueryRepo (InqueryRepo),
    SlackAPI (SlackAPI),
    SlackWebhookURL (SlackWebhookURL),
    countOpen,
    postMessage,
  )

postSlack :: (Has SlackWebhookURL env, HasLogFunc env) => Text -> RIO env ()
postSlack text = do
  SlackWebhookURL url <- view getL
  logInfo $ "postSlack: url=" <> displayShow url <> ", message=" <> display text
  req <-
    parseRequestThrow ("POST " <> url)
      <&> setRequestBodyJSON (object ["text" .= text])
  void $ httpNoBody req

slackAPIImpl :: (Has SlackWebhookURL env, HasLogFunc env) => SlackAPI env
slackAPIImpl = SlackAPI postSlack

countInqueries :: (Has ConnectionPool env, HasLogFunc env) => RIO env Int
countInqueries = do
  ConnectionPool _pool <- view getL
  n <- liftIO $
    withResource _pool $ \conn -> do
      let sql = "SELECT COUNT(*) FROM inquery WHERE status <> 'CLOSED' "
      [Only c] <- query_ conn sql
      pure c
  logInfo $ "countInqueries: " <> display n
  pure n

inqueryRepoImpl :: (Has ConnectionPool env, HasLogFunc env) => InqueryRepo env
inqueryRepoImpl = InqueryRepo countInqueries

app :: (HasLogFunc env, Has1 SlackAPI env, Has1 InqueryRepo env) => RIO env ()
app = do
  n <- runIF $ view countOpen
  let msg = "There are " <> display n <> " open inqueries"
  logInfo $ "msg: " <> msg
  runIF $ \api -> view postMessage api $ textDisplay msg

data AppEnv env = AppEnv (InqueryRepo (AppEnv env)) (SlackAPI (AppEnv env)) (Extends env)

deriveEnv ''AppEnv

instance HasLogFunc env => HasLogFunc (AppEnv env) where
  logFuncL = superL . logFuncL

appImpl :: (HasLogFunc env, Has ConnectionPool env, Has SlackWebhookURL (AppEnv env)) => App env
appImpl = mapBaseRIO (AppEnv inqueryRepoImpl slackAPIImpl . Extends) $ App app

{- (Trans (FindEnv ConnectionPool (Addr env Root))
          (Addr (AppEnv env) (FindEnv ConnectionPool (Addr env Root)))
          (AppEnv env))
  -- Addr (AppEnv env) (FindEnv ConnectionPool (Addr env Root))
  -- AppEnv env : Addr env (FindEnv ConnectionPool (Addr (AppEnv env) Root)
                      -}

{-# LANGUAGE OverloadedStrings #-}

module Tutorial2.App where

import Control.Env.Hierarchical (Has, Has1, getL, runIF)
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
    HasLogFunc,
    MonadIO (liftIO),
    RIO,
    Text,
    displayShow,
    logInfo,
    view,
    void,
    (<&>),
  )
import Tutorial2.Interface

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
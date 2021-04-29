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
    MonadIO (liftIO),
    RIO,
    Text,
    view,
    void,
    (<&>),
  )
import Tutorial2.Interface
  ( ConnectionPool (ConnectionPool),
    InqueryRepo (InqueryRepo),
    SlackAPI (SlackAPI),
    SlackWebhookURL (SlackWebhookURL),
    countOpen,
    postMessage,
  )

postSlack :: (Has SlackWebhookURL env) => Text -> RIO env ()
postSlack text = do
  SlackWebhookURL url <- view getL
  req <-
    parseRequestThrow ("POST " <> url)
      <&> setRequestBodyJSON (object ["text" .= text])
  void $ httpNoBody req

slackAPIImpl :: (Has SlackWebhookURL env) => SlackAPI env
slackAPIImpl = SlackAPI postSlack

countInqueries :: (Has ConnectionPool env) => RIO env Int
countInqueries = do
  ConnectionPool _pool <- view getL
  liftIO $
    withResource _pool $ \conn -> do
      let sql = "SELECT COUNT(*) FROM inquery WHERE status <> 'CLOSED' "
      [Only c] <- query_ conn sql
      pure c

inqueryRepoImpl :: (Has ConnectionPool env) => InqueryRepo env
inqueryRepoImpl = InqueryRepo countInqueries

app :: (Has1 SlackAPI env, Has1 InqueryRepo env) => RIO env ()
app = do
  n <- runIF $ view countOpen
  let msg = "There are " <> display n <> " open inqueries"
  runIF $ \api -> view postMessage api $ textDisplay msg
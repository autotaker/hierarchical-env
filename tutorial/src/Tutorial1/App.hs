{-# LANGUAGE OverloadedStrings #-}

module Tutorial1.App where

import Control.Env.Hierarchical (Has, getL)
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
import Tutorial1.Interface (ConnectionPool (ConnectionPool), SlackWebhookURL (SlackWebhookURL))

postSlack :: (Has SlackWebhookURL env) => Text -> RIO env ()
postSlack text = do
  SlackWebhookURL url <- view getL
  req <-
    parseRequestThrow ("POST " <> url)
      <&> setRequestBodyJSON (object ["text" .= text])
  void $ httpNoBody req

countInqueries :: (Has ConnectionPool env) => RIO env Int
countInqueries = do
  ConnectionPool _pool <- view getL
  liftIO $
    withResource _pool $ \conn -> do
      let sql = "SELECT COUNT(*) FROM inquery WHERE status <> 'CLOSED' "
      [Only c] <- query_ conn sql
      pure c

app :: (Has SlackWebhookURL env, Has ConnectionPool env) => RIO env ()
app = do
  n <- countInqueries
  let msg = "There are " <> display n <> " open inqueries"
  postSlack $ textDisplay msg
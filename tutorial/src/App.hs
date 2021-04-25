{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Env.Hierarchical (Has, getL)
import Data.Aeson
import Data.Pool (withResource)
import Database.MySQL.Simple (Only (Only), query_)
import Interface (ConnectionPool (ConnectionPool), SlackWebhookURL (SlackWebhookURL))
import Network.HTTP.Simple
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

postSlack :: (Has SlackWebhookURL env, HasLogFunc env) => Text -> RIO env ()
postSlack text = do
  SlackWebhookURL url <- view getL
  logInfo $ "postSlack: url=" <> displayShow url <> ", message=" <> display text
  req <-
    parseRequestThrow ("POST " <> url)
      <&> setRequestBodyJSON (object ["text" .= text])
  void $ httpNoBody req

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

app :: (HasLogFunc env, Has SlackWebhookURL env, Has ConnectionPool env) => RIO env ()
app = do
  n <- countInqueries
  let msg = "There are " <> display n <> " open inqueries"
  postSlack $ textDisplay msg
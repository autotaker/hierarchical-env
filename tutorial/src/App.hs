{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Env.Hierarchical (Has, getL)
import Data.Aeson
import Interface (ConnectionPool (ConnectionPool), SlackWebhookURL (SlackWebhookURL))
import Network.HTTP.Simple
import RIO
  ( Display (display, textDisplay),
    HasLogFunc,
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
  req <-
    parseRequestThrow ("POST " <> url)
      <&> setRequestBodyJSON (object ["text" .= text])
  logInfo $ "postSlack: url=" <> displayShow url <> ", message=" <> display text
  void $ httpNoBody req

countInqueries :: (Has ConnectionPool env, HasLogFunc env) => RIO env Int
countInqueries = do
  ConnectionPool _pool <- view getL
  logInfo "countInqueries: 10"
  pure 10

app :: (HasLogFunc env, Has SlackWebhookURL env, Has ConnectionPool env) => RIO env ()
app = do
  n <- countInqueries
  let msg = "There are " <> display n <> " open inqueries"
  postSlack $ textDisplay msg
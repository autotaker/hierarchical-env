module Interface where

import RIO (ByteString)

newtype ConnectionPool = ConnectionPool ()

newtype SlackWebhookURL = SlackWebhookURL String

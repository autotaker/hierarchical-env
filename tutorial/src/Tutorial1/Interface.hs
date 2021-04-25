module Tutorial1.Interface where

import Data.Pool (Pool)
import Database.MySQL.Simple (Connection)

newtype ConnectionPool = ConnectionPool (Pool Connection)

newtype SlackWebhookURL = SlackWebhookURL String

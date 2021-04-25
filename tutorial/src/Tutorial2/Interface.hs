{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial2.Interface where

import Data.Pool (Pool)
import Database.MySQL.Simple (Connection)
import Lens.Micro.Platform (makeLenses)
import RIO (Generic, RIO, Text)

newtype ConnectionPool = ConnectionPool (Pool Connection)

newtype SlackWebhookURL = SlackWebhookURL String

newtype SlackAPI env = SlackAPI
  { _postMessage :: Text -> RIO env ()
  }
  deriving (Generic)

makeLenses ''SlackAPI

newtype InqueryRepo env = InqueryRepo
  { _countOpen :: RIO env Int
  }
  deriving (Generic)

makeLenses ''InqueryRepo
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tutorial2.Interface where

import Data.Pool (Pool)
import Database.MySQL.Simple (Connection)
import Lens.Micro.Platform (makeLenses)
import RIO (RIO, Text)

newtype ConnectionPool = ConnectionPool (Pool Connection)

newtype SlackWebhookURL = SlackWebhookURL String

newtype SlackAPI env = SlackAPI
  { _postMessage :: Text -> RIO env ()
  }

makeLenses ''SlackAPI

newtype InqueryRepo env = InqueryRepo
  { _countOpen :: RIO env Int
  }

makeLenses ''InqueryRepo
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial3.Interface where

import Control.Env.Hierarchical (Interface (IBase))
import Data.Pool (Pool)
import Database.MySQL.Simple (Connection)
import Lens.Micro.Platform (makeLenses)
import RIO (Generic, RIO, Text)

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

newtype App env = App
  { _app :: RIO env ()
  }
  deriving (Generic)

makeLenses ''App

instance Interface App where
  type IBase App = RIO

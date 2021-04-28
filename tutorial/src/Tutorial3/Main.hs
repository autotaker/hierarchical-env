module Tutorial3.Main where

import Control.Env.Hierarchical ( runIF )
import Data.Pool (createPool)
import Database.MySQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectPort,
        connectUser
      ),
    close,
    connect,
    defaultConnectInfo,
  )
import RIO (view, logOptionsHandle, runRIO, stdout, withLogFunc)
import System.Environment (getEnv)
import Tutorial3.Env (mkEnv)
import Tutorial3.Interface
  ( ConnectionPool (ConnectionPool),
    SlackWebhookURL (SlackWebhookURL),
    app,
  )

main :: IO ()
main = do
  logOptions <- logOptionsHandle stdout False
  withLogFunc logOptions $ \lf -> do
    cInfo <- getConnectionInfo
    hook <- getSlackWebhookURL
    pool <- createPool (connect cInfo) close 1 0.5 10
    let env = mkEnv lf (ConnectionPool pool) hook
    runRIO env (runIF (view app))

getSlackWebhookURL :: IO SlackWebhookURL
getSlackWebhookURL =
  SlackWebhookURL <$> getEnv "SLACK_WEBHOOK_URL"

getConnectionInfo :: IO ConnectInfo
getConnectionInfo = do
  host <- getEnv "DB_HOST"
  port <- getEnv "DB_PORT"
  name <- getEnv "DB_NAME"
  user <- getEnv "DB_USER"
  pass <- getEnv "DB_PASS"
  pure $
    defaultConnectInfo
      { connectHost = host,
        connectPort = read port,
        connectDatabase = name,
        connectUser = user,
        connectPassword = pass
      }
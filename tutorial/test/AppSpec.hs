{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AppSpec where

import App
import Control.Concurrent
import Control.Env.Hierarchical
import Control.Monad.Except
import Data.Aeson (KeyValue ((.=)), Value, decode, eitherDecodeStrict', object)
import Data.ByteString.Builder (toLazyByteString)
import Data.Pool
import Database.MySQL.Simple
import Interface
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import RIO
import RIO.ByteString.Lazy (toStrict)
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as LT
import Test.Hspec

data MockWHEnv = MockWHEnv LogFunc SlackWebhookURL

deriveEnv ''MockWHEnv

instance HasLogFunc MockWHEnv where
  logFuncL = getL

data MockDBEnv = MockDBEnv LogFunc ConnectionPool

deriveEnv ''MockDBEnv

instance HasLogFunc MockDBEnv where
  logFuncL = getL

withStdoutLogFunc :: (LogFunc -> IO ()) -> IO ()
withStdoutLogFunc doit = do
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions doit

spec :: Spec
spec = do
  aroundAll withStdoutLogFunc $ do
    describe "postSlack" $
      aroundAllWith withMockAPI $ do
        it "send post request to SlackWebhookURL" $ \lf -> do
          let env = MockWHEnv lf (SlackWebhookURL "http://localhost:10080/webhook")
          runRIO env (postSlack "Hello World!") `shouldReturn` ()

    describe "countInqueries" $
      aroundAllWith withConnectionPool $ do
        beforeWith setupTable $ do
          it "count the number of inqueriew whose status is not close" $ \(lf, cp) -> do
            let env = MockDBEnv lf cp
            runRIO env countInqueries `shouldReturn` 5

setupTable :: (LogFunc, ConnectionPool) -> IO (LogFunc, ConnectionPool)
setupTable (lf, ConnectionPool cp) =
  withResource cp $ \conn -> do
    void $ execute_ conn "DELETE FROM `inquery`"
    void $ executeMany conn "INSERT INTO `inquery`(`title`,`status`) VALUES (?,?)" dat
    pure (lf, ConnectionPool cp)
  where
    dat :: [(Text, Text)]
    dat =
      [ ("title1", "NEW"),
        ("title2", "NEW"),
        ("title3", "IN PROGRESS"),
        ("title4", "IN PROGRESS"),
        ("title5", "IN PROGRESS"),
        ("title6", "CLOSED"),
        ("title7", "CLOSED"),
        ("title8", "CLOSED"),
        ("title9", "CLOSED")
      ]

withConnectionPool :: ActionWith (LogFunc, ConnectionPool) -> ActionWith LogFunc
withConnectionPool doit lf =
  bracket (createPool (connect cInfo) close 1 0.5 1) destroyAllResources $ \cp -> do
    doit (lf, ConnectionPool cp)
  where
    cInfo =
      defaultConnectInfo
        { connectUser = "docker",
          connectPassword = "docker",
          connectDatabase = "test_database",
          connectHost = "127.0.0.1"
        }

withMockAPI :: (LogFunc -> IO ()) -> LogFunc -> IO ()
withMockAPI doit lf = bracket launch shutdown (\_ -> doit lf)
  where
    launch = forkIO (Warp.run 10080 mockAPI)
    shutdown tid = killThread tid

assertJSONBody :: ByteString -> Value -> Either Utf8Builder ()
assertJSONBody text expected = runExcept $ do
  actual <- withExcept fromString (liftEither $ eitherDecodeStrict' text)
  when (actual /= expected) $
    throwError $
      "Expected " <> displayShow expected <> "\n But found: " <> displayShow actual

mockAPI :: Application
mockAPI req respond
  | ["webhook"] <- pathInfo req = do
    body <- toStrict <$> strictRequestBody req
    case assertJSONBody body (object ["text" .= ("Hello World!" :: Text)]) of
      Right _ -> respond $ responseLBS status200 [] "OK"
      Left err ->
        err
          & getUtf8Builder
          & toLazyByteString
          & responseLBS status400 []
          & respond
  | otherwise = respond $ responseLBS status404 [] "Not found"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial1.AppSpec where

import Control.Concurrent (forkIO, killThread)
import Control.Env.Hierarchical (deriveEnv)
import Control.Monad.Except
  ( MonadError (throwError),
    liftEither,
    runExcept,
    withExcept,
  )
import Data.Aeson (KeyValue ((.=)), Value, eitherDecodeStrict', object)
import Data.ByteString.Builder (toLazyByteString)
import Data.Pool (createPool, destroyAllResources, withResource)
import Database.MySQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectUser
      ),
    close,
    connect,
    defaultConnectInfo,
    executeMany,
    execute_,
  )
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai
  ( Application,
    Request (pathInfo),
    responseLBS,
    strictRequestBody,
  )
import qualified Network.Wai.Handler.Warp as Warp
import RIO
  ( ByteString,
    IsString (fromString),
    Text,
    Utf8Builder (getUtf8Builder),
    bracket,
    displayShow,
    runRIO,
    void,
    when,
    (&),
  )
import RIO.ByteString.Lazy (toStrict)
import Test.Hspec
  ( ActionWith,
    Spec,
    aroundAll,
    beforeWith,
    describe,
    it,
    shouldReturn,
  )
import Tutorial1.App (countInqueries, postSlack)
import Tutorial1.Interface
  ( ConnectionPool (ConnectionPool),
    SlackWebhookURL (SlackWebhookURL),
  )

newtype MockWHEnv = MockWHEnv SlackWebhookURL

deriveEnv ''MockWHEnv

newtype MockDBEnv = MockDBEnv ConnectionPool

deriveEnv ''MockDBEnv

spec :: Spec
spec = do
  describe "postSlack" $
    aroundAll withMockAPI $ do
      it "send post request to SlackWebhookURL" $ \() -> do
        let env = MockWHEnv (SlackWebhookURL "http://localhost:10080/webhook")
        runRIO env (postSlack "Hello World!") `shouldReturn` ()

  describe "countInqueries" $
    aroundAll withConnectionPool $ do
      beforeWith setupTable $ do
        it "count the number of inqueriew whose status is not close" $ \cp -> do
          let env = MockDBEnv cp
          runRIO env countInqueries `shouldReturn` 5

setupTable :: ConnectionPool -> IO ConnectionPool
setupTable (ConnectionPool cp) =
  withResource cp $ \conn -> do
    void $ execute_ conn "DELETE FROM `inquery`"
    void $ executeMany conn "INSERT INTO `inquery`(`title`,`status`) VALUES (?,?)" dat
    pure (ConnectionPool cp)
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

withConnectionPool :: ActionWith ConnectionPool -> IO ()
withConnectionPool doit =
  bracket (createPool (connect cInfo) close 1 0.5 1) destroyAllResources $ \cp -> do
    doit (ConnectionPool cp)
  where
    cInfo =
      defaultConnectInfo
        { connectUser = "docker",
          connectPassword = "docker",
          connectDatabase = "test_database",
          connectHost = "127.0.0.1"
        }

withMockAPI :: (() -> IO ()) -> IO ()
withMockAPI doit = bracket launch shutdown (\_ -> doit ())
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
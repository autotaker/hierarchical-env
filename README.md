# hierarchical-env
![Haskell CI](https://github.com/autotaker/hierarchical-env/workflows/Haskell%20CI/badge.svg)

my favorite cabal project example

# Motivation

## RIO and Has-pattern
As an example, let's create a slack bot that notify the number of open inqueries stored in some MySQL database.

In order to implement business logic, we need database connection pool and Slack incomming webhook url.
In RIO-style, these dependencies are injected via Has-pattern (`HasConnectionPool` and `HasSlackWebhook`).  

```haskell
-- Interface.hs
module Interface where

import Data.Pool(Pool)
import Lens.Micro(Lens')
import qualified Database.MySQL.Simple as MySQL

newtype SlackWebhookURL = SlackWebhookURL String

class HasConnectionPool env where
    poolL :: Lens' env (Pool MySQL.Connection)

class HasSlackWebhook env where
    slackWebhookL :: Lens' env SlackWebhookURL
```

In the application code, the dependencies are fetched
via `view` function.

```haskell
-- App.hs
{-# LANGUAGE OverloadedStrings #-}
module App where

import RIO
import Data.Aeson(ToJSON)
import Data.Pool(withResource)
import Database.MySQL.Simple(Only(..), query_)

import Interface

post :: ToJSON a => String -> a -> RIO env (Either HttpException ())
post = ...

postSlack :: (HasSlackWebhook env) => Text -> RIO env (Either Text ())
postSlack msg = do
    SlackWebhookURL url <- view slackWebhookL
    let body = ...
    post url body

countInqueries :: (HasConnectionPool env) => RIO env Int
countInqueries = do
    pool <-  view poolL
    [Only c] <- liftIO $ withResource $ \conn -> 
        query_ conn 
          "SELECT COUNT(*) FROM inqueries WHERE status != 'CLOSED'"
    pure c

app :: (HasSlackWebhook env, HasConnectionPool env) => RIO env ()
app = do
    cInqueries <- countInqueries
    let msg = "There are " <> display cInqueries <> " open inqueries"
    postSlack $ textDisplay msg
```

Dependencies are injected by creating `Env` data 
type and implementing `Has*` instances.  

```haskell
-- Env.hs
{-# LANGUAGE TemplateHaskell #-}
module Env where

import Data.Pool(Pool)
import qualified Database.MySQL.Simple as MySQL
import Lens.Micro.Platform(makeLenses)
import Interface

data Env = Env {
    _slackWebhookURL :: SlackWebhookURL,
    _connectionPool :: Pool MySQL.Connection
}
makeLenses ''Env

instance HasSlackWebhook Env where
    slackWebhookL = slackWebhookURL

instance HasConnectionPool Env where
    poolL = connectionPool

mkEnv :: SlackWebhookURL -> MySQL.ConnectionInfo -> IO Env
mkEnv url info = ...
```

In main module, `app` is run with a production DB and a production webhook.

```haskell
-- Main.hs
module Main where

import RIO

import App
import Env

parseArgs = ...

main :: IO ()
main = do
  (webhookURL, connInfo) <- parseArgs
  env <- mkEnv webhookURL connInfo
  runRIO env app
```

In test module, `postMessage` and `countInqueries` are tested with a mock webhook and a local database.


```haskell
-- AppSpec.hs
module AppSpec where

import RIO
import Data.Pool(Pool)
import qualified Database.MySQL.Simple as MySQL
import Test.Hspec


import Interface
import App


newtype MockAPIEnv = MockAPIEnv {
  _slackWebhookURL :: SlackWebHookURL
}

makeLenses ''MockAPIEnv

instance HasSlackWebhook MockAPIEnv where
    slackWebhookL = slackWebhookURL

newtype MockDBEnv = MockDBEnv {
  _pool :: Pool MySQL.Connection
}

makeLenses ''MockDBEnv

instance HasConnectionPool MockDBEnv where
    poolL = pool

type APIServer = ...

withMockAPI :: (APIServer -> IO ()) -> IO ()
withMockAPI = ...

withMockDB :: (Pool MySQL.Connection -> IO ()) -> IO ()
withMockDB = ...

serverURL :: APIServer -> String
serverURL = ...

Spec :: Spec
Spec = do
  aroundAll withMockAPI $ do
    describe "postSlack" $
      it "post message" $ \server -> do
        let env = MockAPIEnv (serverURL server)
        runRIO env (postMessage "Hello World!")
          `shouldReturn` Right ()

  aroundAll withMockDB $ do
    describe "countInqueries" $ do
      it "return the number of open inqueries" $ \pool -> do
        let env = MockDBEnv pool
        runRIO env countInqueries 
          `shouldReturn` 10
```

The advantage of Has-pattern is that dependencies are
independent of each other. For example,
while testing `postMessage`, we only have to prepare
 mock webhook. (We don't have to prepare a test database.) 


## Refactoring

So far, Has-Pattern is effective. However, when we write
tests for `app`, 
we have to prepare both of a mock webhook and a test database. 
The tests for `app` will be a kind of *integration test*, which does not scale for large code base.

Instead, *unit tests* are appropriate for the situation.
Let's do refactoring. First, we add `SlackAPI` and `InqueryRepo` in `Interface.hs`. 

```haskell
-- Interface.hs
...

newtype SlackAPI env = SlackAPI {
    _postMessage :: Text -> RIO env ()
}

makeLenses ''SlackAPI

newtype InqueryRepo env = InqueryRepo {
    _countInqueries :: RIO env Int
}

makeLenses ''InqueryRepository

class HasSlackAPI env where
    slackAPIL :: Lens' env (SlackAPI env)
    
class HasInqueryRepo env where
    inqueryRepoL :: Lens' env (InqueryRepo env)
```

Compared to `SlackWebhookURL`, `SlackAPI` and `InqueryRepo` are records 
consists of monadic functions and take `env` as a type parameter. 
We call these data types as "interfaces".

Then, let's refactor `App.hs` so that `app` require `HasSlackAPI` and `HasInqueryRepo` instead of `HasSlackWebhook` and `HasConnectionPool`.

```haskell

-- App.hs

...

slackAPIImpl :: (HasSlackWebhook env) => SlackAPI env
slackAPIImpl = SlackAPI {
    _postMessage = postSlack
}


inqueryRepoImpl :: (HasConnection env) => InqueryRepo env
inqueryRepoImpl = InqueryRepo {
    _countInqueries = countInqueries
}

app :: (HasSlackAPI env, HasInqueryRepo env) => RIO env ()
app = do
    slackAPI <- view slackAPIL
    inqueryRepo <- view inqueryRepoL
    cInqueries <- view countInqueries inqueryRepo 
    let msg = "There are " <> display cInqueries <> " open inqueries"
    view postMessage slackAPI msg
```

In production environment, add fields `SlackAPI Env` and `InqueryRepo Env` to `Env` and
inject `slackAPIImpl` and `inqueryRepoImpl`.

```haskell
-- Env.hs

data Env = Env {
    _slackWebhookURL :: SlackWebhookURL,
    _slackAPI :: SlackAPI Env,
    _connectionPool :: Pool MySQL.Connection,
    _inqueryRepo :: InqueryRepo Env
}

makeLenses ''Env

...

instance HasSlackAPI env where
    slackAPIL = slackAPI

instance HasInqueryRepo env where
    inqueryRepoL = inqueryRepo


mkEnv :: SlackWebhookURL -> MySQL.ConnectionInfo -> IO Env
mkEnv url info = do
    pool <- ...
    pure Env {
        _slackWEbhookURL = url,
        _connectionPool = pool,
        _slackAPI = slackAPIImpl,
        _inqueryRepo = inqueryRepoImpl
    }

```

In test environment, we inject mock `SlackAPI` and `InqueryRepo` 
to write unit test for `app`.

```haskell
-- AppSpec.hs
{-# LANGUAGE OverloadeStrings #-}

...

data MockAppEnv = MockAppEnv {
    _slackAPI :: SlackAPI MockAppEnv,
    _inqueryRepo :: InqueryRepo MockAppEnv
}

makeLenses ''MockAppEnv

instance HasSlackAPI env where
    slackAPIL = slackAPI

instance HasInqueryRepo env where
    inqueryRepoL = inqueryRepo

spec :: Spec
spec = do
    ...

    describe "app"
      it "sends slack notification for the number of open inqueries"
        let env = MockAppEnv {
              _slackAPI = SlackAPI {
                _postMessage = \msg -> liftIO $ msg `shouldBe` expectedMessage
              },
              _inqueryRepo = InqueryRepo {
                _countInqueries = pure 10
              }
            }
            expectedMessage = "There are 10 open inqueries"
        runRIO env app
```

## Avoiding fat `Env`

The problem with the current code is that `Env` contains all dependencies as fields.
It's not hard to imagine that `Env` will grow to have **hundreds** of fields!

```haskell
data Env = Env {
    _interface1 :: Interface1 Env,
    _interface2 :: Interface2 Env,
    ...
    _interface300 :: Interface100 Env
}

mkEnv ... = do
  ...
  pure Env{
      _interface1 = ...,
      _interface2 = ...,
      ...
      _interface100 = ...
  }
```

Although I'm not sure there are any limit to the number of data type fields,
the fat `Env` is hard to maintain.

Our approach to avoid fat `Env` is make environments hierarchical.
Let's try to separate `Env` into `BaseEnv` and `ExtEnv` so that

 - `BaseEnv` contains `SlackWebhookURL` and `SlackAPI`.
 - `ExtEnv` contains `BaseEnv`, `Pool Connection` and `InqueryRepo`.  

```haskell
-- App.hs

data BaseEnv = BaseEnv {
    _slackWebhookURL :: SlackWebhookURL,
    _slackAPI :: SlackAPI BaseEnv
} 
makeLenses ''BaseEnv

class HasSlackWebhook BaseEnv where
    slackWebhookL = slackWebhookURL

class HasSlackAPI BaseEnv where
    slackAPIL = slackAPI

data ExtEnv = ExtEnv {
    _base :: BaseEnv,
    _connectionPool :: Pool MySQL.Connection,
    _inqueryRepo :: InqueryRepo ExtEnv
}

makeLenses ''ExtEnv

class HasConnectionPool ExtEnv where
    poolL = connectionPool

class HasInqueryRepo ExtEnv where
    inqueryRepoL = inqueryRepo

class HasSlackWebhook ExtEnv where
    slackWebhookL = base . slackWebhookURL

class HasSlackAPI ExtEnv where
    slackAPIL = base . slackAPI

type Env = ExtEnv
```

Unfortunately, this naive implementation has two drawbacks

1. Cannot inherit interfaces due to a type error.

   ```haskell
   class HasSlackAPI ExtEnv where
    slackAPIL = base . slackAPI 
    -- Type error: Couldn't match ExtEnv and BaseEnv
    --   expected: Lens' ExtEnv (SlackAPI ExtEnv)
    --   actual: Lens' ExtEnv (SlackAPI BaseEnv)
   ```

2. The number of `Has*` instances is **quadratic** to the depth of environment hierarchy.

## Summarize the issues

- In order to write unit tests, many interfaces are introduced and `Env` will be very fat.
- Making environments hierarchical does not work because
  - cannot inherit interfaces
  - The number of `Has*` instance declarations is quadratic to the depth of environment hierarchy.

# Our approach

This library provides the following features:

- `Has x env` type constraint, which is generalization of `HasX` type classes.
- Hierarchical Environments by `Envirnoment env` type class.  
  - Each environment (except `Root` environment) should have a super environment, specified by `Super env` type family. 
  - An environment is a nominal sub-type of it's super environment. That is, `Has a (Super env)` implies `Has a env`.
  - Instances of `Environment env` can be derived by TemplateHaskell.
- `Has1 f env` type constraint for interface `f`, which solves the type error problem.

Let's implement the running example with our approach.

## Application code
First, `App.hs` is implemented as follows. 

```haskell
-- App.hs
import Control.Env.Hierarchical

postSlack :: (Has SlackWebhook env) => Text -> RIO env (Either Text ())
postSlack msg = do
    SlackWebhookURL url <- view (getL @SlackWebhookURL)
    let body = ...
    post url body


countInqueries :: (Has ConnectionPool env) => RIO env Int
countInqueries = do
    pool <-  view (getL @ConnectionPool)
    [Only c] <- liftIO $ withResource $ \conn -> 
        query_ conn 
          "SELECT COUNT(*) FROM inqueries WHERE status != 'CLOSED'"
    pure c

slackAPIImpl = SlackAPI {
    _postMessage = postSlack
}

inqueryRepoImpl = InqueryRepoImpl {
    _countInqueries = countInqueries
}


app :: (Has1 SlackAPI env, Has1 InqueryRepo env) => RIO env ()
app = do
    cInqueries <- runIF (\repo -> view countInqueries repo)
    let msg = "There are " <> display cInqueries <> " open inqueries"
    runIF (\api -> view postMessage api msg)
```
The points are:

- dependency to type `T` is represented by `Has T env` constraint and
the injected value is obtained by `view (getL @T)`. 
- Dependency to interface `I`  is represented by `Has1 I env`, and
the injected interface is used like `runIF (\iface -> action_to_iface)`. 

## Dependency Injection
The next is `Env.hs`.

```haskell
-- Env.hs

data BaseEnv = BaseEnv {
    slackWebhook = SlackWebhookURL,
    slackAPI = SlackAPI BaseEnv
}

type instance Super BaseEnv = Root

deriveEnv ''BaseEnv

data ExtEnv = ExtEnv {
  base = BaseEnv,
  connectionPool = ConnectionPool,
  inqueryRepo = InqueryRepo ExtEnv
}

type instance Super ExtEnv = BaseEnv

deriveEnv ''ExtEnv

mkBaseEnv :: SlackWebhookURL -> BaseEnv
mkBaseEnv webhook = BaseEnv {
    slackWebhook = webhook,
    slackAPI = slackAPIImpl
}

mkExtEnv :: MySQL.ConnectionInfo -> BaseEnv -> IO ExtEnv
mkExtEnv info env = do
    pool <- ...
    pure ExtEnv {
      base = env,
      connectionPool = pool,
      inqueryRepo = inqueryRepoImpl
    } 
```

Pretty cool, isn't it?

`BaseEnv` and `ExtEnv` satisfies the following constraints.

```haskell
Has SlackWebhookURL BaseEnv
Has (SlackAPI BaseEnv) BaseEnv
Has BaseEnv ExtEnv
Has SlackWebhookURL ExtEnv
Has (SlackAPI BaseEnv) ExtEnv
Has ConnectionPool ExtEnv
Has (InqueryRepo ExtEnv) ExtEnv
Has1 SlackAPI BaseEnv
Has1 SlackAPI ExtEnv
Has1 InqueryRepo ExtEnv
```
The points are:

- The only boilerpolates are calling `deriveEnv` and an instance of `Super env`. 
  
  ```
  deriveEnv ''BaseEnv
  type instance Super BaseEnv = Root
  ```
- Because `BaseEnv` does not have actual super environment, the special environment `Root` is specified as the dummy super environment.
- In contrast, because the super environment of `ExtEnv` is `BaseEnv`, `ExtEnv` must have `BaseEnv` as a field and must specify as `type instance Super ExtEnv = BaseEnv`.  

## Test code

Here is an example of test code of `App.hs`.

Compared to original test code, the amount of boilerpolate code is smaller in our approach.

```haskell
--AppSpec.hs

newtype MockDBEnv = MockDBEnv ConnectionPool

deriveEnv ''MockDBEnv
type instance Super MockDBEnv = Root

newtype MockAPIEnv = MockAPIEnv SlackWebhook

deriveEnv ''MockAPIEnv
type instance Super MockAPIEnv = Root

data MockAppEnv = 
  MockAppEnv (SlackAPI MockAppEnv) (InqueryRepo MockAppEnv)

deriveEnv ''MockAppEnv
type instance Super MockAppEnv = Root

withMockAPI = ...

withMockDB = ...

Spec :: Spec
Spec = do
  aroundAll withMockAPI $ do
    describe "postSlack" $
      it "post message" $ \port -> do
        let env = MockAPIEnv ("http://localhost:" <> show port <> "/webhook")
        runRIO env (postMessage "Hello World!")
          `shouldReturn` Right ()

  aroundAll withMockDB $ do
    describe "countInqueries" $ do
      it "return the number of open inqueries" $ \pool -> do
        let env = MockDBEnv pool
        runRIO env countInqueries 
          `shouldReturn` 10

  describe "app" $ do
    it "notify the number of open inqueries with slack" $ do
      let env = MockAppEnv slackAPIMock inqueryRepoMock
          slackAPIMock = SlackAPI postMessageMock
          postMessageMock msg = 
            liftIO $ msg `shouldBe` "There are 10 open inqueries"
          inqueryRepoMock = InqueryRepo countInqueriesMock
          countInqueriesMock = pure 10
      runRIO env app
```

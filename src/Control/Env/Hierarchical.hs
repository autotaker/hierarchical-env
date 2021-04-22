{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module : Control.Env.Hierarchical
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Control.Env.Hierarchical
  ( -- * Getting Fields
    -- $usage:field
    Has,
    getL,

    -- * Invoking interface methods
    -- $usage:interface
    Has1,
    runIF,

    -- * Injecting dependecies

    -- ** With universal environments
    -- $usage:dependency

    -- ** With hierarchical environments
    -- $usage:hierarchical
    Root,
    Super,
    deriveEnv,

    -- * Hiding dependencies
    -- $usage:hiding
    Interface (..),
    mapBaseRIO,
  )
where

import Control.Env.Hierarchical.Internal
  ( Has,
    Has1,
    Root,
    Super,
    getL,
    runIF,
  )
import Control.Env.Hierarchical.TH (deriveEnv)
import Control.Method (Interface (IBase, mapBase), mapBaseRIO)

-- $usage:field
--
-- If a method depends on some value of type @T@,
-- use @'Has' T env@ constraints, and get the value from the environment
-- with @view 'getL'@.
--
--
-- @
-- newtype ServerName = ServerName String
-- printServerName :: ('Has' ServerName env) => RIO env ()
-- printServerName = do
--   ServerName n <- view 'getL'
--   liftIO $ putStrLn $ "ServerName is " <> n
-- @

-- $usage:interface
-- We call a record type whose fields are methods as an interface.
--
-- In the following example, @UserRepo@ is an interface.
--
-- Dependency to an interface @F@ is represented as type constraint @Has1 F env@,
-- methods of the interface can be invoked inside @'runIF'@.
--
-- @
-- data UserRepo env = UserRepo {
--   _createUser :: UserName -> RIO env UserId,
--   _setPassword :: UserId -> Password -> RIO env ()
-- }
-- makeLenses ''UserRepo
--
-- data User = User {
--   _userName :: UserName,
--   _userId :: UserId,
-- } deriving(Eq, Ord, Show)
-- makeLenses ''User
--
-- signup :: ('Has1' UserRepo env) => UserName -> Password -> RIO env User
-- signup name passwd = 'runIF' $ \\userRepo -> do
--   userId <- view createUser userRepo name
--   view setPassword userRepo userId passwd
--   pure User { _userName = name, _userId = userId}
-- @

-- $usage:dependency
--
-- First, define environment @Env@ that
-- contains all dependencies as fields.
--
-- @
-- data Env = Env !ServerName !(UserRepo Env)
-- @
--
-- Then, the following boilerpolate will derive required type constraints.
-- (e.g. @'Has' ServerName Env@, @'Has1' UserRepo Env@)
--
-- @
-- 'deriveEnv' ''Env
-- type instance 'Super' Env = 'Root'
-- @
--
-- Now, you can inject dependency by specifying the actual value of @Env@
-- to the argument of 'runRIO'.
--
-- @
-- mkUserRepo :: DBConfig -> UserRepo Env
-- mkUserRepo = ...
--
-- runApp :: ServerName -> DBConfig -> [UserName] -> IO ()
-- runApp serverName dbConfig users = do
--   let env = Env serverName (mkUserRepo dbConfig)
--   runRIO env $ do
--     printServerName
--     forM_ users $ \userName ->
--       user <- signup userName "password"
--       print user
-- @

-- $usage:hierarchical
-- Instead of resolving the dependency universally,
-- you can extend environments by specifying the super environment.
--
-- In the following example @ExtEnv@ inherits @BaseEnv@.
-- The extended environment is a nominal sub-type of its super environment,
-- that is,
--
-- * @'Has' T ('Super' E)@ implies @'Has' T E@, and
-- * @'Has1' F ('Super' E)@ implies @'Has1' F E@.
--
-- @
-- data BaseEnv = BaseEnv !ServerName !ConnectionPool
--
-- 'deriveEnv' ''BaseEnv
-- type instance 'Super' BaseEnv = 'Root'
--
-- data ExtEnv = ExtEnv !BaseEnv !(UserRepo ExtEnv)
--
-- 'deriveEnv' ''ExtEnv
-- type instance 'Super' ExtEnv = BaseEnv
-- @
--
-- Then, @ExtEnv@ resolves the dependencies.
--
-- @
-- userRepoImpl :: 'Has' ConnectionPool env => UserRepo env
-- userRepoImpl = UserRepo createUserImpl setPaswordImpl
--   where
--   createUserImpl userName = ...
--   setPasswordImpl uid passwd = ...
--
-- runApp :: ServerName -> ConnectionPool -> [UserName] -> IO ()
-- runApp serverName pool users = do
--   let baseEnv = BaseEnv serverName pool
--       extEnv = ExtEnv baseEnv userRepoImpl
--   runRIO extEnv $ do
--     printServerName
--     forM_ users $ \usernm -> do
--       user <- signup usernm "password"
--       liftIO $ print user
-- @

-- $usage:hiding
-- Suppose that we are implementing an interface @AuthHandler@,
-- which handle signin and signup business logic.
--
-- @
-- data AuthHandler env = AuthHandler {
--   _signin :: UserName -> Password -> RIO env User
--   _signup :: UserName -> Password -> RIO env User
-- }
-- makeLenses ''AuthHandler
-- @
--
-- The @authHandlerImpl@ depends on another interface @UserRepo@,
-- which accesses a database to store user information.
--
-- @
-- data UserRepo env = UserRepo {
--  _createUser :: UserName -> RIO env UserId,
--  _setPassword :: UserId -> Password -> RIO env ()
-- }
-- makeLenses ''UserRepo
--
-- userRepoImpl :: (Has ConnectionPool env) => UserRepo env
-- userRepoImpl = ...
--
-- authHandlerImpl :: ('Has1' UserRepo env) => AuthHandler env
-- authHandlerImpl = AuthHandler signinImpl signupImpl
--
-- signupImpl :: (Has1 UserRepo env) => UserName -> Password -> RIO env User
-- signupImpl usernm passwd = ...
--
-- signinImpl :: (Has1 UserRepo env) => UserName -> Password -> RIO env User
-- signinImpl usernm passwd = ...
-- @
--
--
-- Assume that @UserRepo@ is a private interface and should not be exported.
-- Let's refactor @authHandlerImpl@ by using 'mapBaseRIO'.
--
-- @
-- data AuthHandler env = AuthHandler {
--   _signin :: UserName -> Password -> RIO env User
--   _signup :: UserName -> Password -> RIO env User
-- } deriving(Generic)
--
-- instance 'Interface' AuthHandler where
--   type 'IBase' AuthHandler = RIO
--
-- data AuthEnv env = AuthEnv !(UserRepo (AutheEnv env)) !env
-- 'deriveEnv' ''AuthEnv
-- type instance 'Super' (AuthEnv env) = env
--
-- authHandlerImpl :: ('Has' ConnectionPool env) => AuthHandler env
-- authHandlerImpl = 'mapBaseRIO' (AuthEnv userRepoImpl) handler
--   where
--     handler = AuthHandler signinImpl signupImpl
-- @
--
-- Now, the dependency to @UserRepo@ is resolved in the module
-- and hidden from the signature of @authHandlerImpl@

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.HierarchicalSpec where

import Control.Env.Hierarchical
import Control.Env.Hierarchical.Internal (Super)
import Control.Method (Interface (IBase), mapBaseRIO)
import Lens.Micro.TH
import RIO
import Test.Hspec

data Env1 = Env1
  { hoge :: HogeObj Env1,
    param1 :: Param1
  }

data Env2 = Env2
  { env1 :: Env1,
    foo :: FooObj Env2
  }

data Env3 env = Env3 env Param2

data HogeObj env = HogeObj
  { _hogeMethod :: Int -> RIO env Text,
    _fugaMethod :: Bool -> RIO env Bool
  }

data FooObj env = FooObj
  { _fooMethod :: RIO env Text,
    _barMethod :: RIO env Bool
  }
  deriving (Generic)

instance Interface FooObj where
  type IBase FooObj = RIO

newtype Param1 = Param1 Int

newtype Param2 = Param2 Int

makeLenses ''HogeObj
makeLenses ''FooObj

deriveEnv ''Env1
deriveEnv ''Env2
deriveEnv ''Env3

type instance Super Env1 = Root

type instance Super Env2 = Env1

type instance Super (Env3 env) = env

example1 :: (Has1 HogeObj env, Has Param1 env) => RIO env Text
example1 = do
  Param1 n <- view getL
  runIF (\x -> view hogeMethod x n)

example2 :: (Has1 FooObj env) => RIO env Text
example2 = runIF _fooMethod

mkEnv1 :: Env1
mkEnv1 =
  Env1
    { hoge = hogeImpl,
      param1 = Param1 10
    }

mkEnv2 :: Env2
mkEnv2 =
  Env2
    { foo = mapBaseRIO mkEnv3 fooImpl,
      env1 = mkEnv1
    }

mkEnv3 :: env -> Env3 env
mkEnv3 env = Env3 env (Param2 5)

hogeImpl :: HogeObj env
hogeImpl =
  HogeObj
    { _hogeMethod = \n -> pure $ textDisplay n,
      _fugaMethod = \b -> pure $ not b
    }

--
fooImpl :: (Has1 HogeObj env, Has Param2 env) => FooObj env
fooImpl =
  FooObj
    { _fooMethod = do
        Param2 n <- view getL
        runIF (\x -> _hogeMethod x n),
      _barMethod = pure True
    }

spec :: Spec
spec = do
  it "" $ do
    runRIO mkEnv1 example1 `shouldReturn` "10"
  it "" $ do
    runRIO mkEnv2 example2 `shouldReturn` "5"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.Hierarchical.InternalSpec where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1),
    Field (fieldL),
    Has,
    Root,
    Super,
    getL,
    runIF,
  )
import Control.Method (Interface (IBase), mapBaseRIO)
import GHC.Generics (Generic)
import Lens.Micro.Mtl (view)
import Lens.Micro.TH (makeLenses)
import RIO (RIO, runRIO)
import Test.Hspec (Spec, describe, it, shouldBe)

data Obj1 env = Obj1
  { _method1 :: RIO env Int,
    _method2 :: RIO env Int
  }

makeLenses ''Obj1

data Obj2 env = Obj2
  { _method3 :: RIO env Int,
    _method4 :: RIO env Int
  }

makeLenses ''Obj2

newtype Param1 = Param1 Int
  deriving (Eq, Show)

newtype Param2 = Param2 Int
  deriving (Eq, Show)

newtype Param3 = Param3 Int
  deriving (Eq, Show)

data Env1 = Env1
  { _obj1 :: Obj1 Env1,
    _param1 :: Param1
  }

makeLenses ''Env1

data Env2 = Env2
  { _env1 :: Env1,
    _obj2 :: Obj2 Env2,
    _param2 :: Param2
  }

makeLenses ''Env2

instance Environment Env1 where
  type Fields1 Env1 = '[Obj1]
  type Fields Env1 = '[Env1, Obj1 Env1, Param1]

type instance Super Env1 = Root

instance Environment Env2 where
  type Fields1 Env2 = '[Obj2]
  type Fields Env2 = '[Env2, Env1, Obj2 Env2, Param2]

type instance Super Env2 = Env1

instance Field Env1 Env1 where
  fieldL = id

instance Field (Obj1 Env1) Env1 where
  fieldL = obj1

instance Field Param1 Env1 where
  fieldL = param1

instance Field Env2 Env2 where
  fieldL = id

instance Field Env1 Env2 where
  fieldL = env1

instance Field (Obj2 Env2) Env2 where
  fieldL = obj2

instance Field Param2 Env2 where
  fieldL = param2

env1Impl :: Env1
env1Impl =
  Env1
    { _obj1 =
        Obj1
          { _method1 = pure 1,
            _method2 = pure 2
          },
      _param1 = Param1 1
    }

env2Impl :: Env2
env2Impl =
  Env2
    { _obj2 =
        Obj2
          { _method3 = do
              x <- runIF _method1
              y <- runIF _method2
              pure $ x + y,
            _method4 = pure 4
          },
      _param2 = Param2 2,
      _env1 = env1Impl
    }

data Env3 env = Env3 Param3 env

instance Environment (Env3 env) where
  type Fields (Env3 env) = '[Param3]
  type Fields1 (Env3 env) = '[]

type instance Super (Env3 env) = env

instance {-# INCOHERENT #-} Field env (Env3 env) where
  fieldL f (Env3 x1 x2) = fmap (\y2 -> Env3 x1 y2) (f x2)

instance Field Param3 (Env3 env) where
  fieldL f (Env3 x1 x2) = fmap (\y1 -> Env3 y1 x2) (f x1)

newtype Obj3 env = Obj3 (RIO env Int)
  deriving (Generic)

instance Interface Obj3 where
  type IBase Obj3 = RIO

example3 :: Has Param1 env => Obj3 env
example3 = mapBaseRIO (Env3 (Param3 0)) $
  Obj3 $ do
    Param3 n <- view getL
    Param1 m <- view getL
    pure $ n + m

spec :: Spec
spec = do
  describe "getL" $ do
    it "getL @(Obj1 Env1) from Env1" $ do
      n <- runRIO env1Impl $ do
        x <- view (getL @(Obj1 Env1))
        _method1 x
      n `shouldBe` 1
    it "getL @Param1 from Env1" $ do
      n <- runRIO env1Impl $ do
        view (getL @Param1)
      n `shouldBe` Param1 1
    it "getL @(Obj1 Env1) from Env2" $ do
      n <- runRIO env2Impl $ do
        x <- view (getL @(Obj1 Env1))
        env <- view (getL @Env1)
        runRIO env $ _method1 x
      n `shouldBe` 1
    it "getL @Param1 from Env1" $ do
      n <- runRIO env2Impl $ do
        view (getL @Param1)
      n `shouldBe` Param1 1
    it "getL @(Obj2 Env2) from Env2" $ do
      n <- runRIO env2Impl $ do
        x <- view (getL @(Obj2 Env2))
        _method3 x
      n `shouldBe` 3
    it "getL @Param2 from Env2" $ do
      n <- runRIO env2Impl $ do
        view (getL @Param2)
      n `shouldBe` Param2 2
  describe "runIF" $ do
    it "runIF @Obj1 from Env1" $ do
      n <- runRIO env1Impl $ do
        runIF @Obj1 _method1
      n `shouldBe` 1
    it "runIF @Obj1 from Env2" $ do
      n <- runRIO env2Impl $ do
        runIF @Obj1 _method1
      n `shouldBe` 1
    it "runIF @Obj2 from Env2" $ do
      n <- runRIO env2Impl $ do
        runIF @Obj2 _method3
      n `shouldBe` 3
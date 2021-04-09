{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.Hierarchical.InternalSpec where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1, Super),
    Field (fieldL),
    Root,
    getL,
    getObj,
    runObj,
  )
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
  type Super Env1 = Root
  type Fields1 Env1 = '[Obj1]
  type Fields Env1 = '[Env1, Obj1 Env1, Param1]

instance Environment Env2 where
  type Super Env2 = Env1
  type Fields1 Env2 = '[Obj2]
  type Fields Env2 = '[Env2, Env1, Obj2 Env2, Param2]

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
              x <- runObj getObj _method1
              y <- runObj getObj _method2
              pure $ x + y,
            _method4 = pure 4
          },
      _param2 = Param2 2,
      _env1 = env1Impl
    }

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
  describe "getObj/runObj" $ do
    it "runObj (getObj @Obj1) from Env1" $ do
      n <- runRIO env1Impl $ do
        runObj (getObj @Obj1) _method1
      n `shouldBe` 1
    it "runObj (getObj @Obj1) from Env2" $ do
      n <- runRIO env2Impl $ do
        runObj (getObj @Obj1) _method1
      n `shouldBe` 1
    it "runObj (getObj @Obj2) from Env2" $ do
      n <- runRIO env2Impl $ do
        runObj (getObj @Obj2) _method3
      n `shouldBe` 3
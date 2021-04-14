{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.HierarchicalSpec where

import Control.Env.Hierarchical
import Control.Env.Hierarchical.Internal (Environment (Fields, Fields1, Super))
import Data.Functor.Contravariant
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

data Env3 env = Env3
  { super :: env,
    param2 :: Param2
  }

data HogeObj env = HogeObj
  { _hogeMethod :: Int -> RIO env Text,
    _fugaMethod :: Bool -> RIO env Bool
  }

data FooObj env = FooObj
  { _fooMethod :: RIO env Text,
    _barMethod :: RIO env Bool
  }

instance Contravariant FooObj where
  contramap f fooObj =
    FooObj
      { _fooMethod = do
          env' <- asks f
          runRIO env' (_fooMethod fooObj),
        _barMethod = do
          env' <- asks f
          runRIO env' (_barMethod fooObj)
      }

newtype Param1 = Param1 Int

newtype Param2 = Param2 Int

makeLenses ''HogeObj
makeLenses ''FooObj

deriveEnv ''Env1 ''Root
deriveEnv ''Env2 ''Env1

instance Environment (Env3 env) where
  type Super (Env3 env) = env
  type Fields (Env3 env) = '[Env3 env, env, Param2]
  type Fields1 (Env3 env) = '[]

instance Field env (Env3 env) where
  fieldL = lens super (\x y -> x {super = y})

instance Field Param2 (Env3 env) where
  fieldL = lens param2 (\x y -> x {param2 = y})

example1 :: (Has1 HogeObj env, Has Param1 env) => RIO env Text
example1 = do
  Param1 n <- view getL
  runObj getObj (\x -> _hogeMethod x n)

example2 :: (Has1 FooObj env) => RIO env Text
example2 = runObj getObj _fooMethod



mkEnv1 :: Env1
mkEnv1 =
  Env1
    { hoge = hogeImpl,
      param1 = Param1 10
    }

mkEnv2 :: Env2
mkEnv2 =
  Env2
    { foo = contramap mkEnv3 fooImpl,
      env1 = mkEnv1
    }

mkEnv3 :: env -> Env3 env
mkEnv3 env =
  Env3
    { super = env,
      param2 = Param2 5
    }

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
        runObj getObj (\x -> _hogeMethod x n),
      _barMethod = pure True
    }

spec :: Spec
spec = do
  it "" $ do
    runRIO mkEnv1 example1 `shouldReturn` "10"
  it "" $ do
    runRIO mkEnv2 example2 `shouldReturn` "5"
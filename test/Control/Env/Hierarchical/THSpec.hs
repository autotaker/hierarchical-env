{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.Hierarchical.THSpec where

import Control.Env.Hierarchical.Internal
import Control.Env.Hierarchical.TH
import Data.Kind (Type)
import Data.Typeable
import Lens.Micro (Lens')
import Test.Hspec

type F env = (env -> Int)

data Env f a = Env
  { _x :: a,
    _y :: Bool,
    _z :: Maybe (Env f a),
    _w :: f a (Env f a),
    _v :: F (Env f a)
  }

deriveEnv ''Env ''Root

type E = Env Either Int

spec :: Spec
spec = describe "deriveEnv" $ do
  it "`Super E` is Root" $ do
    typeRep (Proxy @(Super E)) `shouldBe` typeRep (Proxy @Root)
  it "`Fields E` is '[E, Int, Bool, Maybe E, Either Int E, F E]" $ do
    typeRep (Proxy @(Fields E))
      `shouldBe` typeRep (Proxy @'[E, Int, Bool, Maybe E, Either Int E, F E])
  it "`Fields1 E` is '[Maybe, Either Int]" $ do
    typeRep (Proxy @(Fields1 E)) `shouldBe` typeRep (Proxy @'[Maybe, Either Int])

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Control.Env.Hierarchical.THSpec where

import Control.Env.Hierarchical.Internal
  ( Environment (Fields, Fields1),
    Extends,
    Field (fieldL),
    Root,
    Super,
  )
import Control.Env.Hierarchical.TH (deriveEnv)
import Data.Maybe (isNothing)
import Data.Typeable (Proxy (Proxy), typeRep)
import Lens.Micro (to, (^.), (^?), _Left)
import Test.Hspec (Spec, describe, it, shouldBe)

type F env = (env -> Int)

data Env f a = Env
  { _x :: a,
    _y :: Bool,
    _z :: Maybe (Env f a),
    _w :: f a (Env f a),
    _v :: F (Env f a) -- Type Synonym is not allowed for Field1
  }

type E = Env Either Int

mkEnv :: E
mkEnv =
  Env
    { _x = 0,
      _y = True,
      _z = Nothing,
      _w = Left 0,
      _v = const 0
    }

deriveEnv ''Env

newtype Param1 = Param1 Int

data Env2 = Env2 (Extends E) Param1

deriveEnv ''Env2

spec :: Spec
spec = describe "deriveEnv" $ do
  it "`Super E` is Root" $ do
    typeRep (Proxy @(Super E)) `shouldBe` typeRep (Proxy @Root)
  it "`Super Env2` is E" $ do
    typeRep (Proxy @(Super Env2)) `shouldBe` typeRep (Proxy @E)
  it "`Fields E` is '[E, Int, Bool, Maybe E, Either Int E, F E]" $ do
    typeRep (Proxy @(Fields E))
      `shouldBe` typeRep (Proxy @'[E, Int, Bool, Maybe E, Either Int E, F E])
  it "`Fields1 E` is '[Maybe, Either Int]" $ do
    typeRep (Proxy @(Fields1 E)) `shouldBe` typeRep (Proxy @'[Maybe, Either Int])

  it "Field E E is defined" $ do
    _x (mkEnv ^. fieldL @E) `shouldBe` (0 :: Int)
  it "Field Int E is defined" $ do
    (mkEnv ^. fieldL @Int) `shouldBe` (0 :: Int)
  it "Field Bool E is defined" $ do
    (mkEnv ^. fieldL @Bool) `shouldBe` True
  it "Field (Maybe E) E is defined" $ do
    (mkEnv ^. fieldL @(Maybe E) . to isNothing) `shouldBe` True
  it "Field (Either Int E) is defined" $ do
    (mkEnv ^? fieldL @(Either Int E) . _Left) `shouldBe` Just 0

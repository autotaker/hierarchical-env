{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Env.Hierarchical.THSpec where

import Control.Env.Hierarchical.Internal
import Control.Env.Hierarchical.TH
import Data.Kind (Type)
import Data.Typeable
import Test.Hspec

data Env a = Env {}

deriveEnv ''Env ''Root

spec :: Spec
spec = describe "deriveEnv" $ do
  it "`Super Env` is Root" $ do
    typeRep (Proxy @(Super (Env Int))) `shouldBe` typeRep (Proxy @Root)
  it "`Fields Env` is '[]" $ do
    typeRep (Proxy @(Fields (Env Int))) `shouldBe` typeRep (Proxy @('[] :: [Type]))

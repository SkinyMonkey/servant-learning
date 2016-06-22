{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Definitions.Track where

import Prelude ()
import Prelude.Compat
import Data.Aeson.Types
import GHC.Generics

data Track = Track
  { name :: String
  , artist :: String
  , url :: String
  , provider :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Track
instance FromJSON Track

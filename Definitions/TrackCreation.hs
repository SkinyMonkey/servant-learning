{-# LANGUAGE DeriveGeneric #-}

module Definitions.TrackCreation (
  trackFromBody,
  TrackCreation
) where

import Prelude ()
import Prelude.Compat
import Data.Maybe
import Data.Aeson.Types
import GHC.Generics

import Definitions.Track

data TrackCreation = TrackCreation
  { name :: String
  , artist :: Maybe String
  , url :: String
  , provider :: String
  } deriving (Eq, Show, Generic)

instance FromJSON TrackCreation

trackFromBody :: TrackCreation -> Track
trackFromBody track_body = Track (Definitions.TrackCreation.name track_body) artist_ (Definitions.TrackCreation.url track_body) (Definitions.TrackCreation.provider track_body)
   where artist_ = fromMaybe "default" $ Definitions.TrackCreation.artist track_body

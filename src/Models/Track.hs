{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models.Track where

import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateTrack"] [persistLowerCase|
TrackRow
    name String
    artist String
    url String
    provider String
    deriving Show
|]

data Track = Track
    { name :: String
    , artist :: String
    , url :: String
    , provider :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Track
instance FromJSON Track

rowToTrack :: TrackRow -> Track
rowToTrack TrackRow{..} = Track {name = trackRowName, artist = trackRowArtist, url = trackRowUrl, provider = trackRowProvider}

-- FIXME : can we use the same notation?
trackToRow :: Track -> TrackRow
trackToRow track = TrackRow (name track) (artist track) (url track) (provider track)

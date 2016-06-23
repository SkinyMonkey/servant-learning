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

module Models where

import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)

-- Persistent models

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
UserRow
    name String
    email String
    deriving Show
|]

share [mkPersist sqlSettings, mkMigrate "migrateTrack"] [persistLowerCase|
TrackRow
    _name String
    _url String
    deriving Show
|]

-- Api models

-- User

data User = User
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

userToUser :: UserRow -> User
userToUser UserRow{..} = User { name = userRowName, email = userRowEmail }

-- Track

-- data Track = Track
--     { name :: String
--     , url :: String
--     } deriving (Eq, Show, Generic)
-- 
-- instance ToJSON Track
-- instance FromJSON Track
-- 
-- trackRowToTrack :: TrackRow -> Track
-- trackRowToTrack TrackRow{..} = Track { name = trackRowName, track = trackRowurl }

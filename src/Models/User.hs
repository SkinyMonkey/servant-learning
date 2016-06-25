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

module Models.User where

import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
UserRow
    name String
    email String
    deriving Show
|]

data User = User
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

rowToUser :: UserRow -> User
rowToUser UserRow{..} = User { name = userRowName, email = userRowEmail }

userToRow :: User -> UserRow
userToRow user = UserRow (name user) (email user)

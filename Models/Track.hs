{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Track (
  InsertTrack,
  GetTracks,
  GetTrack,
  UpdateTrack,
  DeleteTrack
) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

import Definitions.Track
import Definitions.TrackCreation

-- Track is member of the ToJson class?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TrackRow
    name String
    artist String
    url String
    provider String
    deriving Show
|]

insertTrack :: String -> TrackCreation -> App Int64
insertTrack user_id track skip = do
    new_track <- runDb $ insert $ trackToRow track
    return $ fromSqlKey new_track

getTracks :: App [Track]
getTracks = do
    tracks <- runDb (selectList [] [])
    let rows = map (rowToTrack . entityVal) tracks
    return row

getTrack :: String -> App Track
getTrack = do
    maybeTrackRow <- runDb (selectFirst [TrackRowName ==. str] [])
    let maybeTrack = fmap (rowToTrack . entityVal) maybeTrackRow
    case maybeTrack of
         Nothing     -> throwError err404
         Just track -> return track

-- UpdateTrack
-- DeleteTrack

-- FIXME : really needed? seems like boilerplate
-- can we make TrackRow == Track?
-- that would make sense
-- -> see yesod for that
-- seems like it
trackToRow :: Track -> TrackRow
trackToRow track_row = TrackRow (Definitions.Track.name track_row) (Definitions.Track.artist track_row) (Definitions.Track.url track_row) (Definitions.Track.provider track_row)

rowToTrack :: TrackRow -> Track
rowToTrack track_row = Track (trackRowName track_row) (trackRowArtist track_row) (trackRowUrl track_row) (trackRowProvider track_row)

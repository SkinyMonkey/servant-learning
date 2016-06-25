{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Track where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..), App(..))
import           Models.Track
import           Db

type TrackAPI =
         "tracks" :> Get '[JSON] [Track]
    :<|> "tracks" :> Capture "track_id" String :> Get '[JSON] Track
    :<|> "tracks" :> ReqBody '[JSON] Track :> Post '[JSON] Int64

trackServer :: ServerT TrackAPI App
trackServer = allTracks :<|> singleTrack :<|> createTrack

allTracks :: App [Track]
allTracks = do
    tracks <- runDb (selectList [] [])
    let people = map (rowToTrack . entityVal) tracks
    return people


singleTrack :: String -> App Track
singleTrack str = do
    maybeTrackRow <- runDb (selectFirst [TrackRowName ==. str] [])
    let maybeTrack = fmap (rowToTrack . entityVal) maybeTrackRow
    case maybeTrack of
         Nothing     -> throwError err404
         Just track -> return track

createTrack :: Track -> App Int64
createTrack p = do
    newTrack <- runDb (insert (trackToRow p))
    return $ fromSqlKey newTrack

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified Data.Aeson.Parser

import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Definitions.Track
import Definitions.TrackCreation

-- import Models.Track

-- mock data
easy :: Track
easy = Track "Easy" "Son lux" "http://youtube.com/easy" "youtube"

run_for_your_life :: Track
run_for_your_life = Track "Run for your life" "Aries" "http://soundcloud.com/run_for_your_life" "soundcloud"
-- end data

-- We define the API as type combination of routes returning types/model, like in swagger
type TrackAPI = "tracks" :> Get '[JSON] [Track]
           :<|> "tracks" :> Capture "track_id" String  :> Get '[JSON] Track
           :<|> "tracks" :> ReqBody '[JSON] TrackCreation :> Post '[JSON] Track -- String should become an Int for id

-- Note : Maybe can be use to make input optional
-- FIXME : put TrackCreation in a separate module, create a TrackCreation -> Track function and export it

tracks :: Handler [Track]
tracks = return [easy, run_for_your_life]

track :: String -> Handler Track
track track_id
  | track_id == "easy" = return easy
  | track_id == "run_for_your_life" = return run_for_your_life
  | otherwise = throwError $ err404 -- { errBody = "{\"error\": \"Track not found\"}" } -- FIXME how to return JSON

  -- where -- FIXME : check in db if exists

unauthorized :: Handler Track
unauthorized = throwError $ err401

add_track :: TrackCreation -> Handler Track
add_track track_body = do
--      unauthorized;
      return $ trackFromBody track_body

-- creates a Server holding TrackAPI -- GETME
-- combination of handlers to create a TrackAPI like declared
server :: Server TrackAPI
server = tracks
    :<|> track
    :<|> add_track

-- boilerplate .. to do what? -- GETME
trackAPI :: Proxy TrackAPI
trackAPI = Proxy

-- Create a Wai application
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve trackAPI server

-- we run a web server
main :: IO ()
main = run 8081 $ logStdoutDev $ app1 -- FIXME : make the log vary

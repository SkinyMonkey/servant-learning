{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import Servant.JS

import           Config                      (Config (..), App(..))
import           Models
import           Db

type UserAPI =
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64


userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser


allUsers :: App [User]
allUsers = do
    users <- runDb (selectList [] [])
    let people = map (userToUser . entityVal) users
    return people


singleUser :: String -> App User
singleUser str = do
    maybeUserRow <- runDb (selectFirst [UserRowName ==. str] [])
    let maybeUser = fmap (userToUser . entityVal) maybeUserRow
    case maybeUser of
         Nothing     -> throwError err404
         Just user -> return user


createUser :: User -> App Int64
createUser p = do
    newUser <- runDb (insert (UserRow (name p) (email p)))
    return $ fromSqlKey newUser

generateJavaScript :: IO ()
generateJavaScript = writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

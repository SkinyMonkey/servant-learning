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

import           Config                      (Config (..), App(..))
import           Models.User
import           Db

type UserAPI =
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "user_id" String :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

allUsers :: App [User]
allUsers = do
    users <- runDb (selectList [] [])
    let people = map (rowToUser . entityVal) users
    return people

singleUser :: String -> App User
singleUser str = do
    maybeUserRow <- runDb (selectFirst [UserRowName ==. str] [])
    let maybeUser = fmap (rowToUser . entityVal) maybeUserRow
    case maybeUser of
         Nothing     -> throwError err404
         Just user -> return user

createUser :: User -> App Int64
createUser p = do
    newUser <- runDb (insert (userToRow p))
    return $ fromSqlKey newUser

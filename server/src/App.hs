{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Persist.Sqlite as Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text hiding (map)

import           Api
import           Models

server :: ConnectionPool -> Server Api
server pool =      addUser
              :<|> getUser
              :<|> deleteUser
              :<|> getUsers
  where
    addUser user    = liftIO $ addUser' user
    getUser name    = liftIO $ getUser' name
    getUsers        = liftIO $ getUsers'
    deleteUser name = liftIO $ deleteUser' name

    addUser' :: User -> IO (Maybe (Key User))
    addUser' user = flip Sqlite.runSqlPersistMPool pool $ do
      exists <- Sqlite.selectFirst [UserName ==. (userName user)] []
      case exists of
        Nothing -> Just <$> Sqlite.insert user
        Just _ -> return Nothing

    getUser' :: Text -> IO (Maybe User)
    getUser' name = flip runSqlPersistMPool pool $ do
      mUser <- Sqlite.selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

    getUsers' :: IO [User]
    getUsers' = flip runSqlPersistMPool pool $ do
      users <- Sqlite.selectList [] []
      let users' = map (\(Sqlite.Entity _ x) -> x) users
      return users'

    deleteUser' :: Text -> IO ()
    deleteUser' name = flip runSqlPersistMPool pool $ do
      Sqlite.deleteWhere [UserName ==. name]
      return ()



app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    Sqlite.createSqlitePool (cs sqliteFile) 5

  runSqlPool (Sqlite.runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile

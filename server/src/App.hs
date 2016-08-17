{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module App where

import           Control.Monad.IO.Class

import           Control.Monad.Logger (runStderrLoggingT)

import           Control.Monad.Catch (throwM)

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
server pool =      addTodo
              :<|> getTodo
              :<|> updateTodo
              :<|> deleteTodo
              :<|> getTodos
  where
    addTodo todo       = liftIO $ addTodo' todo
    getTodo id         = liftIO $ getTodo' id
    updateTodo id todo = liftIO $ updateTodo' id todo
    getTodos           = liftIO $ getTodos'
    deleteTodo id      = liftIO $ deleteTodo' id

    addTodo' :: Todo -> IO (Key Todo)
    addTodo' todo = flip Sqlite.runSqlPersistMPool pool $ do
      Sqlite.insert todo

    getTodo' :: Key Todo -> IO Todo
    getTodo' key = flip runSqlPersistMPool pool $ do
      todo <- Sqlite.get key
      case todo of
        Just todo -> return todo
        Nothing -> throwM err404

    updateTodo' key todo = flip Sqlite.runSqlPersistMPool pool $ do
      Sqlite.replace key todo
      return NoContent

    getTodos' :: IO [Todo]
    getTodos' = flip runSqlPersistMPool pool $ do
      todos <- Sqlite.selectList [] []
      let todos' = map (\(Sqlite.Entity _ todo) -> todo) todos
      return todos'

    deleteTodo' :: (Key Todo) -> IO NoContent
    deleteTodo' id = flip runSqlPersistMPool pool $ do
      Sqlite.delete id
      return NoContent

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

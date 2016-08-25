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
server pool =      createTodo
              :<|> readTodo
              :<|> updateTodo
              :<|> deleteTodo
              :<|> readTodos
  where
    createTodo todo    = liftIO $ createTodo' todo
    readTodo id        = liftIO $ readTodo' id
    updateTodo id todo = liftIO $ updateTodo' id todo
    deleteTodo id      = liftIO $ deleteTodo' id
    readTodos          = liftIO $ readTodos'

    createTodo' :: Todo -> IO TodoId
    createTodo' todo = flip Sqlite.runSqlPersistMPool pool $ do
      Sqlite.insert todo

    readTodo' :: TodoId -> IO (Maybe Todo)
    readTodo' id = flip Sqlite.runSqlPersistMPool pool $ do
      Sqlite.get id

    updateTodo' :: TodoId -> Todo -> IO NoContent
    updateTodo' id todo = flip Sqlite.runSqlPersistMPool pool $ do
      Sqlite.replace id todo
      return NoContent

    deleteTodo' :: TodoId -> IO NoContent
    deleteTodo' id = flip runSqlPersistMPool pool $ do
      Sqlite.delete id
      return NoContent

    readTodos' :: IO [Todo]
    readTodos' = flip runSqlPersistMPool pool $ do
      todos <- Sqlite.selectList [] []
      let todos' = map (\(Sqlite.Entity _ todo) -> todo) todos
      return todos'

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

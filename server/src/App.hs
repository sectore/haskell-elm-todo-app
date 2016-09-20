{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Api                         (Api, api)
import           Control.Monad.Catch         ()
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.String.Conversions     (cs)
import           Data.Text                   ()
import           Database.Persist.Sql        (ConnectionPool, Entity,
                                              runSqlPersistMPool, runSqlPool,
                                              (==.))
import           Database.Persist.Sqlite     (createSqlitePool, delete, insert,
                                              replace, runMigration,
                                              selectFirst, selectList)
-- import           Models                      (Todo, TodoId, migrateAll)
import           Models
import           Network.Wai                 (Application, Middleware)
import           Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                                              corsRequestHeaders,
                                              simpleCorsResourcePolicy,
                                              simpleMethods)
import           Servant.API
-- import           Servant.API                 ((:<|>), NoContent)
import           Servant.Server              (Server, serve)

server :: ConnectionPool -> Server Api
server pool =      createTodo
              :<|> readTodo
              :<|> updateTodo
              :<|> deleteTodo
              :<|> readTodos
  where
    createTodo todo        = liftIO $ createTodo' todo
    readTodo todoId        = liftIO $ readTodo' todoId
    updateTodo todoId todo = liftIO $ updateTodo' todoId todo
    deleteTodo todoId      = liftIO $ deleteTodo' todoId
    readTodos              = liftIO readTodos'

    createTodo' :: Todo -> IO TodoId
    createTodo' todo = flip runSqlPersistMPool pool $
      insert todo

    readTodo' :: TodoId -> IO (Maybe (Entity Todo))
    readTodo' todoId = flip runSqlPersistMPool pool $
      selectFirst [TodoId ==. todoId] []

    updateTodo' :: TodoId -> Todo -> IO NoContent
    updateTodo' todoId todo = flip runSqlPersistMPool pool $ do
      replace todoId todo
      return NoContent

    deleteTodo' :: TodoId -> IO NoContent
    deleteTodo' todoId = flip runSqlPersistMPool pool $ do
      delete todoId
      return NoContent

    readTodos' :: IO [Entity Todo]
    readTodos' = flip runSqlPersistMPool pool $
      selectList [] []

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ corsMiddleware $ app pool

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just resourcePolicy)
  where
    resourcePolicy = simpleCorsResourcePolicy
      { corsMethods = "DELETE":"PUT":simpleMethods -- simpleMethods are GET,HEAD,POST
      , corsRequestHeaders = ["Content-Type"] }

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile

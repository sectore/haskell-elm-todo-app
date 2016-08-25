{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Test.Hspec
import Test.Hspec.Wai (get, with, shouldRespondWith)

import Network.HTTP.Client
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.Trans.Except
import Servant.API
import Servant.Client
import Test.Mockery.Directory (inTempDirectory)
import Database.Persist
import Database.Persist.Sqlite (toSqlKey)
import App (mkApp)
import Api
import Models


createTodo :: Todo -> Manager -> BaseUrl -> ClientM (Key Todo)
readTodo :: (Key Todo) -> Manager -> BaseUrl -> ClientM (Maybe Todo)
updateTodo :: (Key Todo) -> Todo -> Manager -> BaseUrl -> ClientM NoContent
deleteTodo :: (Key Todo) -> Manager -> BaseUrl -> ClientM NoContent
getTodos :: Manager -> BaseUrl -> ClientM [Todo]

createTodo :<|> readTodo :<|> updateTodo :<|> deleteTodo :<|> getTodos = client api

spec :: Spec
spec = do
  around withApp $ do

    describe "GET /todos" $ do
      it "responds with an empty list by default" $ \ port -> do
        try port getTodos `shouldReturn` []
 
    describe "POST /todo" $ do
      it "responds with a key of new created todo" $ \ port -> do
        let todo = Todo "Do something" True
        id <- try port (createTodo todo)
        -- todo is first entry, so it has to have an id of "1"
        id `shouldBe` toSqlKey (read "1")        

    describe "GET /todo" $ do
      it "responds with Nothing if no todo is available" $ \ port -> do
        let sqlKey = toSqlKey (read "100")
        try port (readTodo sqlKey) `shouldReturn` Nothing
      it "responds with a todo" $ \ port -> do
        let todo = Todo "Do something" True
        id <- try port (createTodo todo)
        try port (readTodo id) `shouldReturn` (Just todo)

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

try :: Int -> (Manager -> BaseUrl -> ClientM a) -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runExceptT $ action manager baseUrl
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a

main :: IO ()
main = hspec spec

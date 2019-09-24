module AppSpec where

import           Test.Hspec
import           Test.Hspec.Wai             (get, shouldRespondWith, with)

import           Api
import           App                        (mkApp)
import           Control.Exception          (ErrorCall (..), throwIO)
import           Control.Monad.Trans.Except
import           Database.Persist
import           Database.Persist.Sqlite    (toSqlKey)
import           Models
import           Network.HTTP.Client
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Client
import           Test.Mockery.Directory     (inTempDirectory)


createTodo :: Todo -> ClientM TodoId
readTodo :: TodoId -> ClientM (Maybe (Entity Todo))
updateTodo :: TodoId -> Todo -> ClientM NoContent
deleteTodo :: TodoId -> ClientM NoContent
getTodos :: ClientM [Entity Todo]

createTodo :<|> readTodo :<|> updateTodo :<|> deleteTodo :<|> getTodos = client api

spec :: Spec
spec =
  around withApp $ do

    describe "GET /todos" $
      it "responds with an empty list by default" $ \ port ->
        try port getTodos `shouldReturn` []

    describe "POST /todo" $
      it "responds with a key of new created todo" $ \ port -> do
        let todo = Todo "Do something" True
        id <- try port (createTodo todo)
        -- todo is first entry, so it has to have an id of "1"
        id `shouldBe` toSqlKey (read "1")

    describe "GET /todo" $ do
      it "responds with Nothing if no todo is available" $ \ port ->
        let sqlKey = toSqlKey (read "100") in
        try port (readTodo sqlKey) `shouldReturn` Nothing
      it "responds with a todo" $ \ port -> do
        let todo = Todo "Do something" True
        id <- try port (createTodo todo)
        let entity = Entity id todo
        try port (readTodo id) `shouldReturn` Just entity

    describe "DELETE /todo" $
      it "updates a todo" $ \port -> do
        let todoA = Todo "Do A" True
        let todoB = Todo "Do B" True
        idA <- try port (createTodo todoA)
        idB <- try port (createTodo todoB)
        let entityB = Entity idB todoB
        try port (deleteTodo idA)
        try port getTodos `shouldReturn` [entityB]

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

try :: Int -> ClientM a -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  -- result <- runExceptT $ action manager baseUrl
  result <- runClientM action $ mkClientEnv manager baseUrl
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a

main :: IO ()
main = hspec spec

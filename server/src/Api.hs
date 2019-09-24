{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import           Database.Persist (Entity)
import           Models           (Todo, TodoId)
import           Servant.API      ((:<|>), (:>), Capture, Delete, Get, JSON,
                                   NoContent, Post, Put, ReqBody)


type Api =
  -- create
       "todo"  :> ReqBody '[JSON] Todo
               :> Post '[JSON] TodoId
  -- read
  :<|> "todo"  :> Capture "key" TodoId
               :> Get '[JSON] (Maybe (Entity Todo))
  -- update
  :<|> "todo"  :> Capture "key" TodoId
               :> ReqBody '[JSON] Todo
               :> Put '[JSON] NoContent
  -- delete
  :<|> "todo"  :> Capture "id" TodoId
               :> Delete '[JSON] NoContent
  -- all
  :<|> "todos" :> Get '[JSON] [Entity Todo]


api :: Proxy Api
api = Proxy

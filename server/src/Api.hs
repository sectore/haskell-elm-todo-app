{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API



type Api =
       "user"  :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user"  :> Capture "name" Text :> Get '[JSON] (Maybe User)
  :<|> "user"  :> Capture "name" Text :> Delete '[JSON] NoContent
  :<|> "users" :> Get '[JSON] [User]
  
  -- create
  :<|> "todo"  :> ReqBody '[JSON] Todo
               :> Post '[JSON] (Key Todo)
  -- read
  :<|> "todo"  :> Capture "key" (Key Todo)
               :> Get '[JSON] Todo
  -- update
  :<|> "todo"  :> Capture "key" (Key Todo)
               :> ReqBody '[JSON] Todo
               :> Put '[JSON] NoContent
  -- delete
  :<|> "todo"  :> Capture "id" (Key Todo)
               :> Delete '[JSON] NoContent
  -- all
  :<|> "todos" :> Get '[JSON] [Todo]


api :: Proxy Api
api = Proxy

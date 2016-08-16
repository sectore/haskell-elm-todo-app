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
  :<|> "todo"  :> ReqBody '[JSON] Todo :> Post '[JSON] (Key Todo)
  -- :<|> "todo"  :> Capture "todoid" Integer :> Get '[JSON] (Maybe Todo)
  -- :<|> "todo"  :> Capture "todoid" Integer :> Delete '[JSON] NoContent
  -- :<|> "todos" :> Get '[JSON] [Todo]


api :: Proxy Api
api = Proxy

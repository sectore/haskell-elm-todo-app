{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text

import           Database.Persist

import           Models

import           Servant.API


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

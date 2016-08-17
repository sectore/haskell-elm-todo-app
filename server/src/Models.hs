{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson
import Data.Text
import GHC.Generics

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    name Text
    age  Int
    UniqueName name
    deriving Eq Read Show
  Todo
    description String
    completed Bool
    deriving Generic Show
|]

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User name age) =
    object [ "name" .= name
           , "age"  .= age]

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \ v ->
    Todo <$> v .: "description"
         <*> v .: "completed"
         
instance ToJSON Todo
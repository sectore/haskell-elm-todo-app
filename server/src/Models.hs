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
{-# LANGUAGE FlexibleInstances          #-}

module Models where

import Data.Aeson
import Data.Text
import GHC.Generics

import Database.Persist.TH
import Database.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    description String
    completed Bool
    deriving Generic Show Read Eq
|]

instance FromJSON Todo
instance FromJSON (Entity Todo) where
  parseJSON = entityIdFromJSON


instance ToJSON Todo
instance ToJSON (Entity Todo) where
  toJSON = entityIdToJSON

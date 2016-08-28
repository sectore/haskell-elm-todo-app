{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

import           Database.Persist
import           Database.Persist.TH

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

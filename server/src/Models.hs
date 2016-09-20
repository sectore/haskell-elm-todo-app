{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON)
import           Database.Persist    (Entity, entityIdFromJSON, entityIdToJSON)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           GHC.Generics        (Generic)

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

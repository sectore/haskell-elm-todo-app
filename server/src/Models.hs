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
  Todo
    description String
    completed Bool
    deriving Generic Show Read Eq
|]

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \ v ->
    Todo <$> v .: "description"
         <*> v .: "completed"
 
instance ToJSON Todo
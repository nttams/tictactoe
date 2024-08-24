{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.Aeson
import qualified GHC.Generics

import Matrix

-- Client type
data ClientRequest = ClientRequest Position Matrix deriving (Eq, Show, GHC.Generics.Generic)
instance Data.Aeson.ToJSON ClientRequest where
instance Data.Aeson.FromJSON ClientRequest where

-- Server types
data State = MACHINE_WON | USER_WON | KEEP_PLAYING deriving (Eq, Show, GHC.Generics.Generic)
instance Data.Aeson.ToJSON State where
instance Data.Aeson.FromJSON State where

data ServerResponse = ServerResponse State Matrix deriving (Eq, Show, GHC.Generics.Generic)
instance Data.Aeson.ToJSON ServerResponse where
instance Data.Aeson.FromJSON ServerResponse where
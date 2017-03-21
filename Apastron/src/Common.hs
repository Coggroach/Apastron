{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Common where

import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bits
import            Data.Bson.Generic
import            Data.Char
import            Data.Time
import            GHC.Generics
import            Servant
import            Data.Text

-----------------------------------------
--  Data Structures
-----------------------------------------

instance FromBSON String
instance ToBSON String

data Vertex = Vertex {
    vId :: String,
    vGroup :: String
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

data Edge = Edge {
    eParent :: Vertex,
    eChild :: Vertex,
    eData :: String
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

data Chain = Chain {
    cEdges :: [Edge]
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

data Graph = Graph {
    gVertices :: [Vertex],
    gEdges :: [Edge]
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

-----------------------------------------
--  User
-----------------------------------------

data User = User {
    uName :: String,
    uAuthToken :: String,
    uHops :: Int
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

-----------------------------------------
--  Response
-----------------------------------------

data Response = Response {
    rData :: String
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

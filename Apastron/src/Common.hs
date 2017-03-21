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

-----------------------------------------
--  Identity
-----------------------------------------

data Identity = Identity {
    iAddress :: String,
    iPort :: String
} deriving (Generic, Eq, Show, ToJSON, FromJSON, ToBSON, FromBSON)

------------------------------
--  Common Functions 
------------------------------

getIdentityPort :: Identity -> Int
getIdentityPort i = read (port i):: Int

getIdentityString :: Identity -> String
getIdentityString i = address i ++ ":" ++ port i

getIdentitySafeString :: Identity -> String
getIdentitySafeString i = address i ++ "_" ++ port i

getIdentityTypeString :: Identity -> String
getIdentityTypeString i = show (serverType i) ++ "_" ++ port i

------------------------------
--  Logging Functions 
------------------------------

logHeading :: String -> IO()
logHeading s = do
    let t = "======================"
    putStrLn t
    putStrLn s
    putStrLn t

logTrailing :: IO ()
logTrailing = putStrLn "======================"

logAction :: String -> String -> String -> IO()
logAction s a m = putStrLn $ "[" ++ s ++ "]" ++ "[" ++ a ++ "]: " ++ m

logError :: String -> String -> IO()
logError s m =  putStrLn $ "[" ++ s ++ "]" ++ "[Error]: " ++ m

logDatabase :: String -> String -> String -> String -> IO()
logDatabase s d a m = putStrLn $ "[" ++ s ++ "]" ++ "[" ++ d ++ ":" ++ a ++ "]: " ++ m

logConnection :: String -> String -> String -> IO()
logConnection c s m = putStrLn $ "[" ++ c ++ "==>>" ++ s ++ "]:" ++ m
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonApi where

import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bits
import            Data.Bson.Generic
import            Data.Char
import            Data.Time
import            GHC.Generics
import            Servant
import            Data.Text
import            Common

-----------------------------------------
--  Identity Data
-----------------------------------------

crawlerIdentity :: Common.Identity
crawlerIdentity = Common.Identity "localhost" "8080"

searchIdentity :: Common.Identity
searchIdentity = Common.Identity "localhost" "8081"

-----------------------------------------
--  Apis
-----------------------------------------

type ApiHandler = ExceptT ServantErr IO

-----------------------------------------
--  Crawler
-----------------------------------------

type CrawlerApi = 
    "init" :> ReqBody '[JSON] Common.User :> Post '[JSON] Common.Response :<|>
    "kill" :> ReqBody '[JSON] Common.User :> Post '[JSON] Common.Response

crawlerApi :: Proxy CrawlerApi
crawlerApi = Proxy

crawlerClientInit :: Common.User -> ClientM Common.Response
crawlerClientKill :: Common.User -> ClientM Common.Response

-----------------------------------------
--  Search
-----------------------------------------

type SearchApi =
    "vertices" :> Get '[JSON] [Common.Vertex] :<|>
    "edges" :> Get '[JSON] [Common.Edge] :<|>
    "graph" :> Get '[JSON] Common.Graph

searchApi :: Proxy SearchApi
searchApi = Proxy

searchClientVertices :: ClientM [Common.Vertex]
searchClientEdges :: ClientM [Common.Edge]
searchClientGraph :: ClientM Common.Graph
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards      #-}

module Search where

import            Common
import            CommonApi
import            MongoDbConnector
import            System.Random
import            System.IO
import            System.Directory
import            Servant
import            Servant.API
import            Servant.Client
import            Servant.JS
import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bson.Generic
import            Data.List.Split
import            Data.List
import            Data.Text
import            Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import            Data.ByteString.Char8 hiding (unpack, putStrLn, find)
import            Data.Time.Clock
import            Data.Map hiding (split)
import            Database.MongoDB
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Resource
import            Control.Monad.IO.Class
import            Control.Monad (when, liftM)
import            Control.Concurrent.STM.TVar
import            Control.Concurrent.STM
import            Control.Concurrent
import            Network.Wai hiding (Response)
import            Network.Wai.Handler.Warp
import            Network.Wai.Logger
import            Network.Wai.Middleware.Cors
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            GitHub
import            GitHub.Data.URL
import            GitHub.Data.Name
import            GitHub.Data.Repos
import            GitHub.Data
import            GitHub.Data.Definitions
import            GitHub.Endpoints.Repos
import            GitHub.Endpoints.Users
import            GitHub.Internal.Prelude
import            GHC.Generics
import            Prelude
import            Boltery

-----------------------------------------
--  Variables
-----------------------------------------

scriptPath :: FilePath
scriptPath = "scripts/api.js"

-----------------------------------------
--  Server
-----------------------------------------

searchServer :: Server SearchApi
searchServer = 
    getGraph

searchApp :: Application
searchApp = serve searchApi searchServer

mkSearch :: IO ()
mkSearch = do
    logHeading "Search"
    logAction "Search" "Starting" $ "on: " ++ getIdentitySafeString searchIdentity
    --writeJSForAPI searchApi jquery scriptPath
    run (getIdentityPort searchIdentity) searchApp

getGraph :: String -> ApiHandler Common.Graph
getGraph username = liftIO $ do
    logAction "Search" "Fetching" "Graph"
    boltCreateGraph $ Data.Text.pack username

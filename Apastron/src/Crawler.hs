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
{-# LANGUAGE RecordWildCards #-} 

module Crawler where

import            Common
import            CommonApi
import            MongoDbConnector
import            System.Random
import            System.IO
import            System.Directory
import            Servant
import            Servant.API
import            Servant.Client
import            Data.Aeson
import            Data.Aeson.TH
import            Data.Bson.Generic
import            Data.List.Split
import            Data.List
import            Data.Text
import            Data.Maybe (catMaybes, fromJust, fromMaybe)
import            Data.ByteString.Char8 hiding (unpack, putStrLn, find)
import            Data.Time.Clock
import            Data.Map hiding (split)
import            Control.Monad.Trans.Except
import            Control.Monad.Trans.Resource
import            Control.Monad.IO.Class
import            Control.Monad (when, liftM)
import            Control.Concurrent.STM.TVar
import            Control.Concurrent.STM
import            Network.Wai hiding (Response)
import            Network.Wai.Handler.Warp
import            Network.Wai.Logger
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            GitHub
import            GitHub.Data.URL
import            GitHub.Data.Name
import            GitHub.Data.Repos
import            GitHub.Data
import            GitHub.Endpoints.Repos
import            GitHub.Endpoints.Users
import            Prelude

-----------------------------------------
--  Variables
-----------------------------------------

data Crawl = Crawl {
    cUsername :: String,
    cStatus :: Bool
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data VertexMap = VertexMap{
    vertices :: TVar (Data.Map Text Common.Vertex)
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data CrawlInfo = CrawlInfo {
    cCrawl :: Crawl,
    cUser :: Common.User,
    cAuth :: Maybe GitHub.Data.Auth
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

-----------------------------------------
--  Server
-----------------------------------------

crawlerServer :: Server CrawlerApi
crawlerServer = 
    initCrawler :<|>
    killCrawler

crawlerApp :: Application
crawlerApp = serve crawlerApi crawlerServer

mkCrawler :: IO ()
mkCrawler = do
    logHeading "Crawler"
    logAction "Crawler" "Starting" $ "on Port: " ++ getIdentityPort crawlerIdentity
    run (getIdentityPort crawlerIdentity) crawlerApp

initCrawler :: Common.User -> ApiHandler Common.Response
initCrawler user:(Common.User n t h) = liftIO $ do
    logAction "Crawler" "Request" "Creating Crawler Request"
    crawl <- findCrawl n
    case crawl of
        [Crawl _ True] -> Common.Response "Running"
        [Crawl _ False] -> Common.Response "Completed"
        _ -> do            
            upsertCrawl (Crawl n True)
            vMap <- liftIO $ newVertexMap
            startCrawl crawl user vMap
            return Common.Response "Started"

-----------------------------------------
--  Helping Functions
-----------------------------------------

findCrawl :: String -> IO Crawl
findCrawl u = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "find" u
    connectToDatabase $ do
        docs <- find (select ["cUserName" =: u] "UserCrawlDb") >>= drainCursor
        return $ mayMaybe (\b -> fromBSON :: Maybe Crawl) docs

upsertCrawl :: Crawl -> IO ()
upsertCrawl c = liftIO $ do
    let u = cUsername c
    connectToDatabase $ upsert (select ["cUserName" =: u] "UserCrawlDb") $ toBSON c

newVertexMap :: IO VertexMap
newVertexMap atomically $ VertexMap <$> newTVar Data.Map.empty

addVertex :: VertexMap -> Text -> Common.Vertex -> STM ()
addVertex VertexMap{..} t = modifyTVar vertices . Data.Map.insert t

lookupVertex :: VertexMap -> Text -> STM (Maybe Common.Vertex)
lookupVertex VertexMap{..} t = Data.Map.lookup t <$> readTVar vertices

startCrawl :: Crawl -> Common.User -> VertexMap -> IO()
startCrawl c u v = liftIO $ do
    let auth = Just $ GitHub.OAuth $ (Data.ByteString.Char8 t)
    let crawlInfo = CrawlInfo c u auth
    liftIO $ forkIO $ crawlRepositories v auth

crawlRepositories :: VertexMap -> CrawlInfo -> IO()
crawlRepositories v ci = do
    let hops = uHops $ cUser ci

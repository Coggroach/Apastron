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
import            Network.HTTP.Client (newManager, defaultManagerSettings)
import            GitHub
import            GitHub.Data.URL
import            GitHub.Data.Name
import            GitHub.Data.Repos
import            GitHub.Data
import            GitHub.Endpoints.Repos
import            GitHub.Endpoints.Users
import            GHC.Generics
import            Prelude
import            Boltery

-----------------------------------------
--  Variables
-----------------------------------------

instance ToBSON Bool
instance FromBSON Bool

data Crawl = Crawl {
    cUsername :: String,
    cStatus :: Bool
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data VertexMap = VertexMap{
    vertices :: TVar (Map Text Text)
}

data CrawlInfo = CrawlInfo {
    cCrawl :: Crawl,
    cUser :: Common.User,
    cAuth :: Maybe GitHub.Data.Auth
}

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
    logAction "Crawler" "Starting" $ "on: " ++ getIdentitySafeString crawlerIdentity
    run (getIdentityPort crawlerIdentity) crawlerApp

initCrawler :: Common.User -> ApiHandler Common.Response
initCrawler user@(Common.User n t h) = liftIO $ do
    logAction "Crawler" "Request" "Creating Crawler Request"
    crawl <- findCrawl n
    case crawl of
        (Crawl _ True) -> return (Common.Response "Running")
        (Crawl _ False) -> return (Common.Response "Completed")
        _ -> do            
            upsertCrawl (Crawl n True)
            vMap <- newVertexMap
            startCrawl crawl user vMap
            return (Common.Response "Started")

killCrawler :: Common.User -> ApiHandler Common.Response
killCrawler u = return (Common.Response "Not Implemented")

-----------------------------------------
--  Helping Functions
-----------------------------------------

findCrawl :: String -> IO Crawl
findCrawl u = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "find" u
    crawl <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["cUserName" =: u] "UserCrawlDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\b -> fromBSON b :: Maybe Crawl) docs
    return $ Data.List.head crawl

upsertCrawl :: Crawl -> IO ()
upsertCrawl c = liftIO $ do
    let u = cUsername c
    logDatabase "Crawler" "UserCrawlDb" "upsert" u
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["cUserName" =: u] "UserCrawlDb") $ toBSON c

upsertCrawlWhenTrue :: String -> IO Bool
upsertCrawlWhenTrue c = liftIO $ do
    crawl <- findCrawl c
    let status = cStatus crawl    
    if status then do
        let newCrawl = Crawl c False
        upsertCrawl newCrawl
        return status
    else
        return status
    

newVertexMap :: IO VertexMap
newVertexMap = atomically $ VertexMap <$> newTVar Data.Map.empty

addVertex :: VertexMap -> Text -> Text -> STM ()
addVertex VertexMap{..} a = modifyTVar vertices . Data.Map.insert a

lookupVertex :: VertexMap -> Text -> STM (Maybe Text)
lookupVertex VertexMap{..} t = Data.Map.lookup t <$> readTVar vertices

startCrawl :: Crawl -> Common.User -> VertexMap -> IO()
startCrawl c u@Common.User{..} v = liftIO $ do
    let auth = Just $ GitHub.OAuth $ Data.ByteString.Char8.pack uAuthToken
    let crawlInfo = CrawlInfo c u auth
    liftIO $ forkIO $ crawlRepositories v crawlInfo
    return ()



crawlRepositories :: VertexMap -> CrawlInfo -> IO()
crawlRepositories v ci = do
    let hops = uHops $ cUser ci
    let name = uName $ cUser ci
    let tName = Data.Text.pack name
    if hops > 0 then do
        logAction "Crawler" "UserName" name
        hasSeen <- atomically $ lookupVertex v tName
        case hasSeen of
            Just u -> logAction "Crawler" "Already Stored" name
            Nothing -> do
                userInfo <- GitHub.Endpoints.Users.userInfoFor' (cAuth ci) (GitHub.Data.Name.N tName)
                case userInfo of
                    Left err -> do
                        logError "Crawler" $ show err
                        return ()
                    Right userInfo' -> do
                        result <- boltStoreUser userInfo'
                        if result then
                            logAction "Crawler" "Stored" name
                        else logError "Crawler" "Failed to Store User"
                        atomically $ addVertex v tName tName
        --repos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName name) GitHub.Data.Repos.RepoPublicityAll

    else
        do
        logAction "Crawler" "Repos" "Complete"
        return ()

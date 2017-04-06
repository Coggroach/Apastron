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
    cName :: String,
    cStatus :: Bool
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data LookupMap = LookupMap{
    entries :: TVar (Map Text String)
}

data CrawlInfo = CrawlInfo {
    cUserName :: Text,
    cAuthToken :: Maybe GitHub.Data.Auth,
    cHops :: Int,
    cRepoName :: Text,
    cContributions :: Int,
    cLanguage :: Text
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
            vMap <- newLookupMap
            startCrawl crawl user vMap
            return (Common.Response "Started")

killCrawler :: Common.User -> ApiHandler Common.Response
killCrawler u = return (Common.Response "Not Implemented")

-----------------------------------------
--  Db Functions
-----------------------------------------

findCrawl :: String -> IO Crawl
findCrawl u = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "find" u
    crawl <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["cName" =: u] "UserCrawlDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\b -> fromBSON b :: Maybe Crawl) docs
    return $ Data.List.head crawl

upsertCrawl :: Crawl -> IO ()
upsertCrawl c@Crawl{..} = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "upsert" u
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["cName" =: cName] "UserCrawlDb") $ toBSON c

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

-----------------------------------------
--  Lookup Functions
-----------------------------------------
newLookupMap :: IO LookupMap
newLookupMap = atomically $ LookupMap <$> newTVar Data.Map.empty

addLookup :: LookupMap -> Text -> STM ()
addLookup LookupMap{..} a = modifyTVar entries . Data.Map.insert a

findLookup :: LookupMap -> Text -> STM (Maybe Text)
findLookup LookupMap{..} t = Data.Map.lookup t <$> readTVar entries

-----------------------------------------
--  Crawling Functions
-----------------------------------------
startCrawl :: Crawl -> Common.User -> LookupMap -> IO()
startCrawl c u@Common.User{..} lm = liftIO $ do
    let cAuth = Just $ GitHub.OAuth $ Data.ByteString.Char8.pack uAuthToken
    let crawlInfo = CrawlInfo (Data.Text.pack cUsername) cAuth (uHops u) Data.Text.empty -1 Data.Text.empty
    liftIO $ forkIO $ crawlEngine lm crawlInfo
    return ()

crawlOnUser :: LookupMap -> CrawlInfo -> Github.Data.Users.User -> IO()
crawlOnUser lm ci@CrawlInfo{..} user = do
    let name = untagName $ userLogin user
    logAction "Crawler" "UserName" $ show name
    hasSeen <- atomically $ findLookup lm name
    case hasSeen of
        Just u -> logAction "Crawler" "Already Stored" $ show name
        Nothing -> do
            result <- boltStoreUser userInfo'
            logBoolAction result "Crawler" "Stored" $ show name
            atomically $ addLookup lm name
            --userInfo <- GitHub.Endpoints.Users.userInfoFor' (cAuth ci) (GitHub.Data.Name.N cUserName)
            --case userInfo of
            --   Left err -> logError "Crawler" $ show err
            --    Right userInfo' -> do
            --repos <- GitHub.Endpoints.Repos.userRepos (mkOwnerName cUserName) GitHub.Data.Repos.RepoPublicityAll
            --let newCi = CrawlInfo cUserName cAuthToken (cHops - 1) cRepoName cContributions cLanguage
            --mapM_ (crawlOnRepo lm newCi) repos
    when (cLanguage != Data.Text.empty) $ 
        boltStoreUserLanguageLink cUserName cLanguage
    when (cRepoName != Data.Text.empty && cContributions != -1) $ 
        boltStoreUserRepoCollabLink cUserName cRepoName cContributions

crawlOnContributor :: LookupMap -> CrawlInfo -> Github.Data.Repos.Contributor -> IO ()
crawlOnContributor lm ci@CrawlInfo{..} contributor = do
    let contributorName = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributor
    userInfo <- GitHub.Endpoints.Users.userInfoFor' (cAuth ci) (GitHub.Data.Name.N contributorName)
    crawlOnUser lm ci userInfo

crawlOnRepo :: LookupMap -> CrawlInfo -> GitHub.Data.Repos.Repo -> IO ()
crawlOnRepo lm ci@CrawlInfo{..} repo = do
    let repoHTML = getUrl $ GitHub.Data.Repos.repoHtmlUrl repo
    logAction "Crawler" "Repo" repoHTML
    hasSeen <- atomically $ findLookup lm repoHTML
    case hasSeen of
        Just u -> logAction "Crawler" "Already Stored" ""
        Nothing -> do
            result <- boltStoreRepo repo
            logBoolAction result "Crawler" "Stored" repoHtml
            atomically $ addLookup lm repoHTML
            --let newCi = CrawlInfo cUserName cAuthToken (cHops - 1) repoHTML cContributions cLanguage
            --let lang = GitHub.Data.Repos.repoLanguage repo
            --crawlOnLanguage lm newCi lang
    when (cRepoName != Data.Text.empty) $
        boltStoreUserRepoOwnerLink cUserName cRepoName

crawlOnLanguage :: LookupMap -> CrawlInfo -> GitHub.Data.Repos.Language -> IO()
crawlOnLanguage lm ci@CrawlInfo{..} lang = do
    let langText = getLanguage lang
    logAction "Crawler" "Repo" langText
    hasSeen <- atomically $ findLookup lm langText
    case hasSeen of
        Just u -> logAction "Crawler" "Already Stored" ""
        Nothing -> do
            result <- boltStoreLanguage langText
            logBoolAction result "Crawler" "Stored" langText
            atomically $ addLookup lm langText
    when (langText != Data.Text.empty && cRepoName != Data.Text.empty) $
        boltStoreRepoLanguageLink cRepoName langText

crawlUser :: LookupMap -> CrawlInfo -> IO()
crawlUser lm ci = do


crawlEngine :: LookupMap -> CrawlInfo -> IO()
crawlEngine v ci = do
    let hops = uHops $ cUser ci
    if hops > 0 then
        crawlOnUser v ci
    else
        do
        logAction "Crawler" "Repos" "Complete"
        return ()

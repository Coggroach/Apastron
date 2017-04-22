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
import            GitHub.Auth
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

instance ToBSON Bool
instance FromBSON Bool

data Crawl = Crawl {
    cName :: String,
    cStatus :: Bool
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data Token = Token {
    tName :: String,
    tValue :: String
} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data LookupMap = LookupMap{
    entries :: TVar (Map Text String)
}

data CrawlInfo = CrawlInfo {
    cUserName :: Text,
    cAuthToken :: Maybe GitHub.Data.Auth,
    cHops :: Int
}

-----------------------------------------
--  Server
-----------------------------------------

crawlerServer :: Server CrawlerApi
crawlerServer = 
    initCrawler :<|>
    killCrawler :<|>
    storeToken

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
    --crawl <- findCrawl n
    --case crawl of
    --    (Crawl _ True) -> return (Common.Response "Running")
    --    (Crawl _ False) -> return (Common.Response "Completed")
    --    _ -> do            
    let crawl = Crawl n True
    upsertCrawl crawl
    vMap <- newLookupMap
    startCrawl crawl user vMap
    return (Common.Response "Started")

killCrawler :: Common.User -> ApiHandler Common.Response
killCrawler u = liftIO $ do
    upsertCrawlWhenTrue $ uName u
    return (Common.Response "Stopped")

storeToken :: String -> String -> ApiHandler Common.Response
storeToken u t = liftIO $ do
    logDatabase "Crawler" "TokenCrawlDb" "upsert" $ u ++ ":" ++ t
    let token = Crawler.Token u t
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["tName" =: u] "TokenCrawlDb") $ toBSON token
    return (Common.Response "Token Stored")

findToken :: String -> IO String
findToken u = liftIO $ do
    logDatabase "Crawler" "TokenCrawlDb" "find" u
    t <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["tName" =: u] "TokenCrawlDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe Crawler.Token) docs
    if Prelude.null t then do
        logError "Crawler" "No Token Found..."
        return ""
    else 
        do
        let first = Data.List.head t
        return $ Crawler.tValue first

-----------------------------------------
--  Db Functions
-----------------------------------------

findCrawl :: String -> IO Crawl
findCrawl u = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "find" u
    crawl <- connectToDatabase $ do
        docs <- Database.MongoDB.find (Database.MongoDB.select ["cName" =: u] "UserCrawlDb") >>= drainCursor
        return $ Data.Maybe.mapMaybe (\ b -> fromBSON b :: Maybe Crawl) docs
    return $ Data.List.head crawl

upsertCrawl :: Crawl -> IO ()
upsertCrawl c@Crawl{..} = liftIO $ do
    logDatabase "Crawler" "UserCrawlDb" "upsert" cName
    connectToDatabase $ Database.MongoDB.upsert (Database.MongoDB.select ["cName" =: cName] "UserCrawlDb") $ toBSON c

upsertCrawlWhenTrue :: String -> IO Bool
upsertCrawlWhenTrue c = liftIO $ do
    crawl <- findCrawl c
    let status = cStatus crawl
    when status $ do
        let newCrawl = Crawl c False
        upsertCrawl newCrawl
    return status

-----------------------------------------
--  Lookup Functions
-----------------------------------------
newLookupMap :: IO LookupMap
newLookupMap = atomically $ LookupMap <$> newTVar Data.Map.empty

addLookup :: LookupMap -> Text -> String -> STM ()
addLookup LookupMap{..} a = modifyTVar entries . Data.Map.insert a

findLookup :: LookupMap -> Text -> STM (Maybe String)
findLookup LookupMap{..} t = Data.Map.lookup t <$> readTVar entries

-----------------------------------------
--  Crawling Functions
-----------------------------------------
startCrawl :: Crawl -> Common.User -> LookupMap -> IO()
startCrawl Crawl{..} u@Common.User{..} lm = liftIO $ do
    --let cAuth = Just $ GitHub.OAuth $ Data.ByteString.Char8.pack uAuthToken
    --let cAuth = Just $ GitHub.Auth.BasicAuth (Data.ByteString.Char8.pack clientId) (Data.ByteString.Char8.pack clientSecret)
    token <- findToken cName
    let cAuth = if token == "" then Nothing else Just $ GitHub.OAuth $ Data.ByteString.Char8.pack token
    let crawlInfo = CrawlInfo (Data.Text.pack cName) cAuth uHops
    liftIO $ forkIO $ crawlEngine lm crawlInfo
    return ()

crawlOnUser :: LookupMap -> GitHub.Data.Definitions.User -> IO()
crawlOnUser lm user = do
    let name = untagName $ userLogin user
    logAction "Crawler" "UserName" $ show name
    hasSeen <- atomically $ findLookup lm name
    case hasSeen of
        Just u -> logAction "Crawler" "Stored:True" $ show name
        Nothing -> do
            result <- boltStoreUser user
            logBoolAction result "Crawler" "Stored" $ show name
            atomically $ addLookup lm name "User"

logLinkAction :: Bool -> Text -> Text -> IO()
logLinkAction r a b = logBoolAction r "Crawler" "Stored Link" $ show a ++ ":" ++ show b

crawlOnContributor :: LookupMap -> Maybe Auth -> Text -> Text -> GitHub.Data.Repos.Contributor -> IO ()
crawlOnContributor lm a repo lang contributor@(GitHub.Data.Repos.KnownContributor contributions _ _ _ _ _) = do
    let contributorName = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser contributor
    userInfo <- GitHub.Endpoints.Users.userInfoFor' a (GitHub.Data.Name.N contributorName)
    case userInfo of 
        Left err -> do
            logError "Crawler" $ show err
            return ()
        Right user -> crawlOnUser lm user
    langLink <- boltStoreUserLanguageLink contributorName lang
    repoLink <- boltStoreUserRepoCollabLink contributorName repo contributions
    logLinkAction langLink contributorName lang
    logLinkAction repoLink contributorName repo

crawlOnRepo :: LookupMap -> Text -> GitHub.Data.Repos.Repo -> IO ()
crawlOnRepo lm user repo = do
    let repoHtml = getUrl $ GitHub.Data.Repos.repoHtmlUrl repo
    logAction "Crawler" "Repo" $ unpack repoHtml
    hasSeen <- atomically $ findLookup lm repoHtml
    case hasSeen of
        Just u -> logAction "Crawler" "Stored:True" ""
        Nothing -> do
            result <- boltStoreRepo repo
            logBoolAction result "Crawler" "Stored" $ unpack repoHtml
            atomically $ addLookup lm repoHtml "Repo"
    repoLink <- boltStoreUserRepoOwnerLink user repoHtml
    logLinkAction repoLink user repoHtml

crawlOnLanguage :: LookupMap -> Text -> GitHub.Data.Repos.Language -> IO()
crawlOnLanguage lm repo lang = do
    let langText = getLanguage lang
    logAction "Crawler" "Repo" $ unpack langText
    hasSeen <- atomically $ findLookup lm langText
    case hasSeen of
        Just u -> logAction "Crawler" "Stored:True" ""
        Nothing -> do
            result <- boltStoreLanguage langText
            logBoolAction result "Crawler" "Stored" $ unpack langText
            atomically $ addLookup lm langText "Language"
    repoLink <- boltStoreRepoLanguageLink repo langText
    logLinkAction repoLink repo langText

crawlUserToRepos :: LookupMap -> Maybe Auth -> Text -> Int -> IO()
crawlUserToRepos lm a cUserName hops = when (hops > 0) $ do
    eitherUser <- GitHub.Endpoints.Users.userInfoFor' a $ GitHub.Data.Name.N cUserName
    case eitherUser of
        Left err -> logError "Crawler" $ show err
        Right user -> crawlOnUser lm user
    repos <- userRepos' a (mkOwnerName cUserName) RepoPublicityAll
    case repos of
        Left err -> do
            logError "Crawler" $ show err
            return ()
        Right repos' -> do
            mapM_ (crawlOnRepo lm cUserName) repos'
            mapM_ (crawlRepoToLanguage lm) repos'
            mapM_ (crawlRepoToContributors lm a hops) repos'

crawlRepoToLanguage :: LookupMap -> Repo -> IO()
crawlRepoToLanguage lm r@Repo{..} =
    case repoLanguage of 
        Just lang -> crawlOnLanguage lm (getUrl repoHtmlUrl) lang
        Nothing -> return ()

crawlRepoToContributors :: LookupMap -> Maybe Auth -> Int -> Repo -> IO()
crawlRepoToContributors lm a hops r@Repo{..} = do
    let owner = mkOwnerName $ untagName $ simpleOwnerLogin repoOwner
    coUsers <- contributors' a owner repoName
    case coUsers of
        Left err -> do
            logError "Crawler" $ show err
            return ()
        Right coUsers' -> do
            case repoLanguage of
                Just lang -> mapM_ (crawlOnContributor lm a (getUrl repoHtmlUrl) (getLanguage lang)) coUsers'
                Nothing -> logAction "Crawler" "End" ""
            mapM_ (crawlContributorToUser lm a hops) coUsers'

crawlContributorToUser :: LookupMap -> Maybe Auth -> Int -> Contributor -> IO()
crawlContributorToUser lm a hops c = do
    let name = untagName $ simpleUserLogin $ fromJust $ contributorToSimpleUser c
    crawlUserToRepos lm a name $ hops - 1

crawlEngine :: LookupMap -> CrawlInfo -> IO()
crawlEngine lm ci@CrawlInfo{..} = do
    crawlUserToRepos lm cAuthToken cUserName cHops
    status <- upsertCrawlWhenTrue $ unpack cUserName
    logBoolAction status "Crawler" "Stopped" ""
    logTrailing
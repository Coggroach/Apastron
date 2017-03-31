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
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-} 

module Boltery where

import            Database.Bolt
import            Data.Map
import            Data.Text
import            Data.List
import            Data.Maybe
import            Data.Time.Clock
import            GitHub
import            GitHub.Data.URL
import            GitHub.Data.Name
import            GitHub.Data.Repos
import            GitHub.Data
import            GitHub.Data.Definitions
import            GitHub.Endpoints.Repos
import            GitHub.Endpoints.Users
import            GitHub.Internal.Prelude
import            Common

-----------------------------------------
--  Variables
-----------------------------------------

defaultTextNull :: Text
defaultTextNull = Data.Text.pack "NULL"

defaultIntNull :: Int
defaultIntNull = 0

-----------------------------------------
--  Common Functions
-----------------------------------------

boltStore :: Text -> Map Text Database.Bolt.Value -> IO Bool
boltStore c p = do
    let config = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "Coggroach" }
    pipe <- Database.Bolt.connect config
    records <- Database.Bolt.run pipe $ Database.Bolt.queryP c p
    Database.Bolt.close pipe
    return (Data.List.null records)

maybeToValueT :: Maybe Text -> Database.Bolt.Value
maybeToValueT t = Database.Bolt.T $ fromMaybe defaultTextNull t

maybeToValueI :: Maybe Int -> Database.Bolt.Value
maybeToValueI i = Database.Bolt.I $ fromMaybe 0 i

maybeDateToValueI :: Maybe  UTCTime -> Database.Bolt.Value
maybeDateToValueI g = Database.Bolt.I $ floor $ utctDayTime $ fromJust g :: Int

dateToValueI ::  UTCTime -> Database.Bolt.Value
dateToValueI g = Database.Bolt.I $ floor $ utctDayTime g :: Int

-----------------------------------------
--  User
-----------------------------------------

boltCypherUser :: Text
boltCypherUser = 
    Data.Text.pack $ 
    "CREATE (n:User { " ++
    " name: {userLogin}, " ++
    " userLogin: {userLogin}, " ++
    " userId: {userId}, " ++
    " userUrl: {userUrl}, " ++
    " userName: {userName}, " ++
    " userCompany: {userCompany}, " ++
    " userLocation: {userLocation}, " ++
    " userEmail: {userEmail}, " ++
    " userBio: {userBio}, " ++
    " userPublicRepos: {userPublicRepos}, " ++
    " userPublicGists: {userPublicGists}, " ++
    " userFollowers: {userFollowers}, " ++
    " userFollowing: {userFollowing} } )"

boltParamsUser :: GitHub.Endpoints.Users.User -> Map Text Database.Bolt.Value
boltParamsUser GitHub.Endpoints.Users.User{..} = Data.Map.fromList [
        ("userLogin", Database.Bolt.T $ untagName userLogin),
        ("userId", Database.Bolt.I $ untagId userId),
        ("userUrl", Database.Bolt.T $ getUrl userUrl),
        ("userName", maybeToValueT userName),
        ("userCompany", maybeToValueT userCompany),
        ("userLocation", maybeToValueT userLocation),
        ("userEmail", maybeToValueT userEmail),
        ("userBio", maybeToValueT userBio),
        ("userPublicRepos", Database.Bolt.I userPublicRepos),
        ("userPublicGists", Database.Bolt.I userPublicGists),
        ("userFollowers", Database.Bolt.I userFollowers),
        ("userFollowing", Database.Bolt.I userFollowing)]

boltStoreUser :: GitHub.Endpoints.Users.User -> IO Bool
boltStoreUser o = boltStore boltCypherUser $ boltParamsUser o

-----------------------------------------
--  Repos
-----------------------------------------

boltCypherRepo :: Text
boltCypherRepo = 
    Data.Text.pack $ 
    "MERGE (n:Repo { " ++
    " name: {repoHtmlUrl}, " ++
    " repoDescription: {repoDescription}, " ++
    " repoId: {repoId}, " ++
    " repoUrl: {repoUrl}, " ++
    " repoName: {repoName}, " ++
    " repoOwnerLogin: {repoOwnerLogin}, " ++
    " repoOwnerId: {repoOwnerId}, " ++
    " repoPrivate: {repoPrivate}, " ++
    " repoHtmlUrl: {repoHtmlUrl}, " ++
    " repoForks: {repoForks}, " ++
    " repoWatchers: {repoWatchers}, " ++
    " repoSize: {repoSize}, " ++
    " repoOpenIssues: {repoOpenIssues}, " ++
    " repoPushedAt: {repoPushedAt}, " ++
    " repoUpdatedAt: {repoUpdatedAt}, " ++
    " repoCreatedAt: {repoCreatedAt} } )"

boltParamsRepo :: Repo -> Map Text Database.Bolt.Value
boltParamsRepo Repo{..}
     = Data.Map.fromList [
        ("repoDescription", maybeToValueT repoDescription),
        ("repoId", Database.Bolt.I $ untagId repoId),
        ("repoUrl", Database.Bolt.T $ getUrl repoUrl),
        ("repoName", Database.Bolt.T $ untagName repoName),
        ("repoOwnerLogin", Database.Bolt.T $ untagName $ simpleOwnerLogin repoOwner),
        ("repoOwnerId", Database.Bolt.I $ untagId $ simpleOwnerId repoOwner),
        ("repoPrivate", Database.Bolt.B repoPrivate),
        ("repoHtmlUrl", Database.Bolt.T $ getUrl repoHtmlUrl),
        ("repoForks", maybeToValueI repoForks),
        ("repoWatchers", maybeToValueI repoWatchers),
        ("repoSize", maybeToValueI repoSize),
        ("repoOpenIssues", maybeToValueI repoOpenIssues),
        ("repoPushedAt", maybeDateToValueI repoPushedAt),
        ("repoUpdatedAt", maybeDateToValueI repoUpdatedAt),
        ("repoCreatedAt", maybeDateToValueI repoCreatedAt)]

boltStoreRepo :: Repo -> IO Bool
boltStoreRepo r = boltStore boltCypherRepo $ boltParamsRepo r

-----------------------------------------
--  Languages
-----------------------------------------

boltCypherLanguage :: Text
boltCypherLanguage =
    Data.Text.pack $
    "CREATE (n:Language { " ++
    " name: {language} } )"

boltParamsLanguage :: Text -> Map Text Database.Bolt.Value
boltParamsLanguage language
     = Data.Map.fromList [("language", Database.Bolt.T language)]

boltStoreLanguage :: Text -> IO Bool
boltStoreLanguage l = boltStore boltCypherLanguage $ boltParamsLanguage l
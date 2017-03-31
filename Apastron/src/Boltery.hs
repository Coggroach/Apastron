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
{-# LANGUAGE ScopedTypeVariables #-}

module Boltery where

import            Database.Bolt
import            Data.Map
import            Data.Text
import            Data.List
import            GitHub
import            GitHub.Data.URL
import            GitHub.Data.Name
import            GitHub.Data.Repos
import            GitHub.Data
import            GitHub.Endpoints.Repos
import            GitHub.Endpoints.Users
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

boltStore :: Text -> Map Text Value -> IO Bool
boltStore c p = do
    let config = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "Coggroach" }
    pipe <- Database.Bolt.connect config
    records <- Database.Bolt.run pipe $ Database.Bolt.queryP c p
    Database.Bolt.close pipe
    return (null records)

maybeToValueT :: Maybe Text -> Database.Bolt.T
maybeToValueT = fromMaybe defaultTextNull

maybeToValueI :: Maybe Int -> Database.Bolt.I
maybeToValueI = fromMaybe 0

maybeDateToValueI :: Maybe GithubDate -> Database.Bolt.I
maybeDateToValueI g = floor $ utctDayTime (fromJust g) :: Int

dateToValueI :: GithubDate -> Database.Bolt.I
dateToValueI g = floor $ utctDayTime g :: Int

-----------------------------------------
--  User
-----------------------------------------

boltCypherUser :: Text
boltCypherUser = 
    Data.Text.pack $ 
    "CREATE (n:User { " ++
    " name: {detailedOwnerLogin}, " ++
    " detailedOwnerLogin: {detailedOwnerLogin}, " ++
    " detailedOwnerId: {detailedOwnerId}, " ++
    " detailedOwnerUrl: {detailedOwnerUrl}, " ++
    " detailedOwnerName: {detailedOwnerName}, " ++
    " detailedOwnerCompany: {detailedOwnerCompany}, " ++
    " detailedOwnerLocation: {detailedOwnerLocation}, " ++
    " detailedOwnerEmail: {detailedOwnerEmail}, " ++
    " detailedOwnerBio: {detailedOwnerBio}, " ++
    " detailedOwnerPublicRepos: {detailedOwnerPublicRepos}, " ++
    " detailedOwnerPublicGists: {detailedOwnerPublicGists}, " ++
    " detailedOwnerFollowers: {detailedOwnerFollowers}, " ++
    " detailedOwnerFollowing: {detailedOwnerFollowing} } )"

boltParamsUser :: DetailedOwner -> Map Text Value
boltParamsUser DetailedOwner{..} = Data.Map.fromList [
        ("detailedOwnerLogin", Database.Bolt.T detailedOwnerLogin),
        ("detailedOwnerId", Database.Bolt.I detailedOwnerId),
        ("detailedOwnerUrl", Database.Bolt.T detailedOwnerUrl),
        ("detailedOwnerName", maybeToValueT detailedOwnerName),
        ("detailedOwnerCompany", maybeToValueT detailedOwnerCompany),
        ("detailedOwnerLocation", maybeToValueT detailedOwnerLocation),
        ("detailedOwnerEmail", maybeToValueT detailedOwnerEmail),
        ("detailedOwnerBio", maybeToValueT detailedOwnerBio),
        ("detailedOwnerPublicRepos", Database.Bolt.I detailedOwnerPublicRepos),
        ("detailedOwnerPublicGists", Database.Bolt.I detailedOwnerPublicGists),
        ("detailedOwnerFollowers", Database.Bolt.I detailedOwnerFollowers),
        ("detailedOwnerFollowing", Database.Bolt.I detailedOwnerFollowing)]

boltStoreUser :: DetailedOwner -> IO Bool
boltStoreUser o = boltStore boltCypherUser $ boltParamsUser o

-----------------------------------------
--  Repos
-----------------------------------------

boltCypherUser :: Text
boltCypherUser = 
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

boltParamsUser :: Repo -> Map Text Value
boltStoreUser Repo{..}
     = Data.Map.fromList [
        ("repoDescription", maybeToValueT repoDescription),
        ("repoId", Database.Bolt.I repoId),
        ("repoUrl", Database.Bolt.T repoUrl),
        ("repoName", Database.Bolt.T repoName),
        ("repoOwnerLogin", Database.Bolt.T $ githubOwnerLogin repoOwner),
        ("repoOwnerId", Database.Bolt.I $ githubOwnerId repoOwner),
        ("repoPrivate", Database.Bolt.B repoPrivate),
        ("repoHtmlUrl", Database.Bolt.T repoHtmlUrl),
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

boltParamsLanguage :: Text -> Map Text Value
boltParamsLanguage language
     = Data.Map.fromList [("language", Database.Bolt.T language)]

boltStoreLanguage :: Text -> IO Bool
boltStoreLanguage l = boltStore boltCypherLanguage $ boltParamsLanguage l
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
    records <- boltGetRecords c p
    return (Data.List.null records)

boltGetRecords :: Text -> Map Text Database.Bolt.Value -> IO [Database.Bolt.Record]
boltGetRecords c p = do
    let config = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "Coggroach" }
    pipe <- Database.Bolt.connect config
    records <- Database.Bolt.run pipe $ Database.Bolt.queryP c p
    Database.Bolt.close pipe
    return records

maybeToValueT :: Maybe Text -> Database.Bolt.Value
maybeToValueT t = Database.Bolt.T $ fromMaybe defaultTextNull t

maybeToValueI :: Maybe Int -> Database.Bolt.Value
maybeToValueI i = Database.Bolt.I $ fromMaybe 0 i

maybeDateToValueI :: Maybe  UTCTime -> Database.Bolt.Value
maybeDateToValueI g = Database.Bolt.I (floor $ utctDayTime (fromJust g) :: Int)

dateToValueI ::  UTCTime -> Database.Bolt.Value
dateToValueI g = Database.Bolt.I ( floor $ utctDayTime g :: Int )

-----------------------------------------
--  Data Conversions
-----------------------------------------

boltRecordToVertex :: Database.Bolt.Record -> IO Common.Vertex
boltRecordToVertex r = do
    n :: Text <- (r `Database.Bolt.at` "name") >>= Database.Bolt.exact
    g :: Text <- (r `Database.Bolt.at` "group") >>= Database.Bolt.exact
    return (Common.Vertex (Data.Text.unpack n) (Data.Text.unpack g))

boltRecordToEdge :: Database.Bolt.Record -> IO Common.Edge
boltRecordToEdge r = do
    p :: Text <- (r `Database.Bolt.at` "parent") >>= Database.Bolt.exact
    c :: Text <- (r `Database.Bolt.at` "child") >>= Database.Bolt.exact
    g :: Text <- (r `Database.Bolt.at` "type") >>= Database.Bolt.exact
    return (Common.Edge (Data.Text.unpack p) (Data.Text.unpack c) (Data.Text.unpack g))

boltCreateGraph :: Text -> IO Common.Graph
boltCreateGraph t = do
    recordNodes <- boltStoreExtractNode t
    recordLinks <- boltStoreExtractLink t
    vertices <- mapM boltRecordToVertex recordNodes
    edges <- mapM boltRecordToEdge recordLinks
    return (Common.Graph vertices edges)

-----------------------------------------
--  Data Extractions
-----------------------------------------

boltCypherExtractNode :: Text
boltCypherExtractNode =
    Data.Text.pack $
    "MATCH (n) WHERE n.name = {username}" ++
    " OPTIONAL MATCH path=(n)-[*1..2]-(c)" ++
    " RETURN DISTINCT c.name as name, HEAD(LABELS(c)) as group"

boltStoreExtractNode :: Text -> IO [Database.Bolt.Record]
boltStoreExtractNode u = boltGetRecords boltCypherExtractNode $ boltParamsExtract u

boltCypherExtractLink :: Text
boltCypherExtractLink =
    Data.Text.pack $
    "MATCH (n) WHERE n.name = {username}" ++
    "OPTIONAL MATCH path=(n)-[*1..2]-(c)" ++
    " WITH rels(path) AS rels" ++
    " UNWIND rels AS rel" ++ 
    " WITH DISTINCT rel" ++
    " RETURN startnode(rel).name as parent, endnode(rel).name as child, type(rel) as type"

boltParamsExtract :: Text -> Map Text Database.Bolt.Value
boltParamsExtract u = Data.Map.fromList [("username", Database.Bolt.T u)]

boltStoreExtractLink :: Text -> IO [Database.Bolt.Record]
boltStoreExtractLink u = boltGetRecords boltCypherExtractLink $ boltParamsExtract u

-----------------------------------------
--   Store User
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
--  Store Repos
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
--   Store Languages
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

-----------------------------------------
--  Repo<->Language
-----------------------------------------

boltCypherRepoLanguageLink :: Text
boltCypherRepoLanguageLink =
    Data.Text.pack $
    "MATCH (u:Repo {" ++
    "repoHtmlUrl: {repoHtmlUrl} } )," ++
    "(r:Language {" ++
    "name: {language} } ) \n" ++
    "CREATE (u)-[:RepoLanguageLink]->(r)"

boltParamsRepoLanguageLink :: Text -> Text -> Map Text Database.Bolt.Value
boltParamsRepoLanguageLink r l 
    = Data.Map.fromList [
        ("repoHtmlUrl", Database.Bolt.T r),
        ("language", Database.Bolt.T l)]

boltStoreRepoLanguageLink :: Text -> Text -> IO Bool
boltStoreRepoLanguageLink r l = boltStore boltCypherRepoLanguageLink $ boltParamsRepoLanguageLink r l

-----------------------------------------
--  User<->Language
-----------------------------------------

boltCypherUserLanguageLink :: Text
boltCypherUserLanguageLink =
    Data.Text.pack $
    "MATCH (u:User {" ++
    "userLogin: {userLogin} } )," ++
    "(r:Language {" ++
    "name: {language} } ) \n" ++
    "CREATE (u)-[:UserLanguageLink]->(r)"

boltParamsUserLanguageLink :: Text -> Text -> Map Text Database.Bolt.Value
boltParamsUserLanguageLink u l 
    = Data.Map.fromList [
        ("userLogin", Database.Bolt.T u),
        ("language", Database.Bolt.T l)]

boltStoreUserLanguageLink :: Text -> Text -> IO Bool
boltStoreUserLanguageLink r l = boltStore boltCypherUserLanguageLink $ boltParamsUserLanguageLink r l

-----------------------------------------
--  User<->Repo - Owner
-----------------------------------------

boltCypherUserRepoOwnerLink :: Text
boltCypherUserRepoOwnerLink =
    Data.Text.pack $
    "MATCH (u:User {" ++
    "userLogin: {userLogin} } )," ++
    "(r:Repo {" ++
    "repoHtmlUrl: {repoHtmlUrl} } ) \n" ++
    "CREATE (u)-[:UserRepoOwnerLink]->(r)"

boltParamsUserRepoOwnerLink :: Text -> Text -> Map Text Database.Bolt.Value
boltParamsUserRepoOwnerLink u r 
    = Data.Map.fromList [
        ("userLogin", Database.Bolt.T u),
        ("repoHtmlUrl", Database.Bolt.T r)]

boltStoreUserRepoOwnerLink :: Text -> Text -> IO Bool
boltStoreUserRepoOwnerLink u r = boltStore boltCypherUserRepoOwnerLink $ boltParamsUserRepoOwnerLink u r

-----------------------------------------
--  User<->Repo - Collab
-----------------------------------------

boltCypherUserRepoCollabLink :: Text
boltCypherUserRepoCollabLink =
    Data.Text.pack $
    "MATCH (u:User {" ++
    "userLogin: {userLogin} } )," ++
    "(r:Repo {" ++
    "repoHtmlUrl: {repoHtmlUrl} } ) \n" ++
    "CREATE (u)-[c:UserRepoCollabLink {commits: {commits}}]->(r)"

boltParamsUserRepoCollabLink :: Text -> Text -> Int -> Map Text Database.Bolt.Value
boltParamsUserRepoCollabLink u l c
    = Data.Map.fromList [
        ("userLogin", Database.Bolt.T u),
        ("language", Database.Bolt.T l),
        ("commits", Database.Bolt.I c)]

boltStoreUserRepoCollabLink :: Text -> Text -> Int -> IO Bool
boltStoreUserRepoCollabLink u r c = boltStore boltCypherUserRepoCollabLink $ boltParamsUserRepoCollabLink u r c



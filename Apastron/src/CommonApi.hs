{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommonApi where


import            Common
import            Control.Monad.Trans.Except
import            Data.Aeson
import            GHC.Generics
import            Servant
import            Servant.API
import            Servant.Client
import            Network.Wai
import            Network.Wai.Handler.Warp
import            Network.HTTP.Client (newManager, defaultManagerSettings)

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
    "kill" :> ReqBody '[JSON] Common.User :> Post '[JSON] Common.Response :<|>
    "token" :> Capture "u" String :> Capture "t" String :> Get '[JSON] Common.Response

crawlerApi :: Proxy CrawlerApi
crawlerApi = Proxy

crawlerClientInit :: Common.User -> ClientM Common.Response
crawlerClientKill :: Common.User -> ClientM Common.Response
crawlerClientToken :: String -> String -> ClientM Common.Response

crawlerClientInit :<|> crawlerClientKill :<|> crawlerClientToken = Servant.Client.client crawlerApi

-----------------------------------------
--  Search
-----------------------------------------

type SearchApi =
    "graph" :> Capture "username" String :> Get '[JSON] Common.Graph :<|>
    "language" :> Get '[JSON] Common.Languages :<|>
    "favourite" :> Get '[JSON] Common.Graph

searchApi :: Proxy SearchApi
searchApi = Proxy

searchClientGraph :: String -> ClientM Common.Graph
searchClientLanguages :: ClientM Common.Languages
searchClientFavourite :: ClientM Common.Graph

searchClientGraph :<|> searchClientLanguages :<|> searchClientFavourite = Servant.Client.client searchApi
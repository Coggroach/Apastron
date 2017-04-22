module Handler.Profile where

import Import
import GitHub.Endpoints.Users
import qualified Servant as S
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings, httpLbs)
import Network.HTTP.Types.Status (statusCode)
import Data.Text.Encoding
import Data.Maybe

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        let username = "Coggroach"
        Right usr <- liftIO $ userInfoFor username
        let name = userLogin usr
        Just nm <- lookupSession "login"
        Just token <- lookupSession "access_token"
        Right currUser <- liftIO $ userInfoCurrent' (OAuth $ Data.Text.Encoding.encodeUtf8 token)
        let createdAt = userCreatedAt currUser
        call <- liftIO $ callCrawler username $ unpack token
        setTitle . toHtml $ nm <> "'s User Page"
        $(widgetFile "profile")

callCrawler :: String -> String -> IO ()
callCrawler un tkn = do
  manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  request <- parseRequest $ "http://localhost:8080/token/" ++ un ++ "/" ++ tkn
  response <- Network.HTTP.Client.httpLbs request manager
  return ()
  --putStrLn $ pack $ show $ Servant.Client.responseBody response

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Conduit (runConduit, (.|), MonadThrow)
import Data.Conduit (ConduitM)
import qualified Data.ByteString.Internal as B
import Network.HTTP.Conduit (http, newManager, tlsManagerSettings, Manager, parseRequest, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.XML.Unresolved (sinkDoc)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as SP
import Servant.API ((:>), QueryParam, Get, JSON, FromHttpApiData(..), Capture, (:<|>)(..))
import Servant.Server (Server, serve, Handler, err404)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.Proxy (Proxy(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import qualified Item
import qualified ParseFeed
import qualified Action
import qualified Interpreter

main :: IO ()
main = run 8081 app where
  app = simpleCors $ serve proxy server
  proxy :: Proxy API
  proxy = Proxy

parseFeed :: String -> IO [Item.Item]
parseFeed url =
  parseRequest url >>= (\request ->
    runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> pure (ParseFeed.transformXMLToItems doc)

type ItemAPI = "items" :> Capture "url" String :> QueryParam "sortBy" SortBy :> Get '[JSON] [Item.Item]
type ProgressAPI = "progress" :> QueryParam "userID" Action.UserID :> QueryParam "url" Action.Url :> Get '[JSON] Action.Progress
type API = ItemAPI :<|> ProgressAPI

data SortBy = PublishedDescending | PublishedAscending

server :: Server API
server = items :<|> progress

items :: String -> Maybe SortBy -> Handler [Item.Item]
items url _ = liftIO (parseFeed url)

progress :: Maybe Action.UserID -> Maybe Action.Url -> Handler Action.Progress
progress (Just uid) (Just url) = do
  position <- liftIO $ foldFree Interpreter.interpret (Action.fetchPosition uid url)
  case position of
    Nothing -> throwError err404
    Just a -> pure a

instance FromHttpApiData SortBy where
  parseQueryParam _ = Right PublishedDescending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings


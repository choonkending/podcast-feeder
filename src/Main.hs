{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.STM (atomically)
import qualified Item
import qualified ParseFeed
import qualified Action
import qualified Interpreter

main :: IO ()
main = do
  tvarDatabase <- atomically Interpreter.newDatabase
  let interpreter = Interpreter.interpret tvarDatabase
  let app = simpleCors $ serve apiProxy (server interpreter)
  run 8081 app

apiProxy :: Proxy API
apiProxy = Proxy

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

server :: Interpreter.Interpreter -> Server API
server interpreter = items :<|> (progress interpreter)

items :: String -> Maybe SortBy -> Handler [Item.Item]
items url _ = liftIO (parseFeed url)

progress :: Interpreter.Interpreter -> Maybe Action.UserID -> Maybe Action.Url -> Handler Action.Progress
progress interpreter (Just uid) (Just url) = do
  position <- liftIO $ foldFree interpreter (Action.fetchPosition uid url)
  case position of
    Nothing -> throwError err404
    Just a -> pure a

instance FromHttpApiData SortBy where
  parseQueryParam _ = Right PublishedDescending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings


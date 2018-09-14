{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Conduit (runConduit, (.|), MonadThrow)
import Data.Conduit (ConduitM)
import qualified Data.ByteString.Internal as B
import Network.HTTP.Conduit (http, newManager, tlsManagerSettings, Manager, parseRequest, Request, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.XML.Unresolved (sinkDoc)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as SP
import qualified Data.Text as T
import Servant.API ((:>), QueryParam, Get, JSON, FromHttpApiData(..), Capture)
import Servant.Server (Server, serve)
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(..))
import Data.Maybe (maybeToList)
import qualified Item

main :: IO ()
main = run 8081 app where
  app = serve proxy server
  proxy :: Proxy ItemAPI
  proxy = Proxy

parseFeed :: String -> IO [Item.Item]
parseFeed url =
  createRequest url >>= (\request ->
    runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> pure (transformXMLToItems doc)

rss :: String -> XT.Name
rss localName = XT.Name {
    XT.nameLocalName = T.pack localName,
    XT.nameNamespace = Nothing,
    XT.namePrefix = Nothing
  }

transformXMLToItems :: XT.Document -> [Item.Item]
transformXMLToItems doc = let
    root = XT.documentRoot doc
    channel = XT.elementChildren root >>= XT.isNamed (rss "channel")
    items = channel >>= XT.elementChildren >>= XT.isNamed (rss "item") >>= transformToItem
  in items

transformToItem :: XT.Element -> [Item.Item]
transformToItem i = do
  title <- XT.elementChildren i >>= XT.isNamed (rss "title") >>= XT.elementText
  description <- XT.elementChildren i >>= XT.isNamed (rss "description") >>= XT.elementText
  enclosure <- XT.elementChildren i >>= XT.isNamed (rss "enclosure") >>= transformToEnclosure
  [Item.Item { Item.title = title, Item.description = description, Item.enclosure = enclosure }]

transformToEnclosure :: XT.Element -> [Item.Enclosure]
transformToEnclosure e = do
  url <- maybeToList $ XT.attributeText (rss "url") e
  length <- maybeToList $ XT.attributeText (rss "length") e >>= parseInteger
  mediaType <- maybeToList $ XT.attributeText (rss "type") e
  [Item.Enclosure { Item.url = url, Item.length = length, Item.mediaType = mediaType }]

parseInteger :: T.Text -> Maybe Integer
parseInteger t = case reads (T.unpack t) of
  [(i, "")] -> Just i
  _ -> Nothing

type ItemAPI = "items" :> Capture "url" String :> QueryParam "sortBy" SortBy :> Get '[JSON] [Item.Item]

data SortBy = PublishedDescending | PublishedAscending

server :: Server ItemAPI
server url _ = liftIO (parseFeed url)

instance FromHttpApiData SortBy where
  parseQueryParam _ = Right PublishedDescending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: String -> IO Request
createRequest url = parseRequest url


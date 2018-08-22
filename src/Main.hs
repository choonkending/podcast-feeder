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
import Text.XML.Unresolved (sinkDoc)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as SP
import Servant.API ((:>), QueryParam, Get, JSON, FromHttpApiData(..))
import Servant.Server (Server, serve)
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(..))

main :: IO ()
main = run 8081 app where
  app = serve proxy server
  proxy :: Proxy RecipeAPI
  proxy = Proxy

parseFeed :: IO ()
parseFeed =
  createRequest >>= (\request ->
    runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> print doc

type RecipeAPI = "recipes" :> QueryParam "sortBy" SortBy :> Get '[JSON] [Recipe]

data Recipe = Recipe {
  title :: String,
  summary :: String
} deriving (Generic)
instance ToJSON Recipe
data SortBy = PublishedDescending | PublishedAscending

recipes1 :: [Recipe]
recipes1 =
  [
    Recipe "Summer Rolls With Jicama, Watermelon, and Herbs" "Keep cool with these refreshing no-cook summer rolls, filled with watermelon, jicama, and herbs. "
  ]

server :: Server RecipeAPI
server _ = return recipes1


instance FromHttpApiData SortBy where
  parseQueryParam _ = Right PublishedDescending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: IO Request
createRequest = parseRequest "http://feeds.feedburner.com/seriouseats/recipes"


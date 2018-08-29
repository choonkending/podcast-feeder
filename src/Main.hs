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

parseFeed :: IO [Recipe]
parseFeed =
  createRequest >>= (\request ->
    runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> pure (transformXMLToRecipe doc)

atom :: String -> XT.Name
atom localName = XT.Name {
    XT.nameLocalName = T.pack localName,
    XT.nameNamespace = Just $ T.pack "http://www.w3.org/2005/Atom" ,
    XT.namePrefix = Nothing
  }

entryToRecipe :: XT.Element -> [Recipe]
entryToRecipe e = do
  title <- XT.elementChildren e >>= XT.isNamed (atom "title") >>= XT.elementText
  summary <- XT.elementChildren e >>= XT.isNamed (atom "summary") >>= XT.elementText
  [Recipe { title = title, summary = summary }]

transformXMLToRecipe :: XT.Document -> [Recipe]
transformXMLToRecipe doc = let
    root = XT.documentRoot doc
    nodes = XT.elementNodes root
    recipes = nodes >>= XT.isElement >>= XT.isNamed (atom "entry") >>= entryToRecipe
  in recipes

type RecipeAPI = "recipes" :> QueryParam "sortBy" SortBy :> Get '[JSON] [Recipe]

data Recipe = Recipe {
  title :: T.Text,
  summary :: T.Text
} deriving (Generic, Show)
instance ToJSON Recipe
data SortBy = PublishedDescending | PublishedAscending

server :: Server RecipeAPI
server _ = liftIO parseFeed


instance FromHttpApiData SortBy where
  parseQueryParam _ = Right PublishedDescending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: IO Request
createRequest = parseRequest "http://feeds.feedburner.com/seriouseats/recipes"


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
import Servant.API ((:>), QueryParam, Get, JSON)
import GHC.Generics (Generic)

main :: IO ()
main =
  createRequest >>= (\request ->
    runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> print doc

type RecipeAPI = "recipes" :> QueryParam "sortBy" SortBy :> Get '[JSON] [Recipe]

data Recipe = Recipe deriving (Generic)
data SortBy = PublishedDescending | PublishedAscending

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| sinkDoc SP.def

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: IO Request
createRequest = parseRequest "http://feeds.feedburner.com/seriouseats/recipes"


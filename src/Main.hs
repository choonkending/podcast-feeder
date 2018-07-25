module Main where
import Conduit (runConduit, (.|), MonadThrow)
import Data.Conduit (ConduitM)
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Internal as B
import Network.HTTP.Conduit (http, newManager, tlsManagerSettings, Manager, parseRequest, Request, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Text.XML.Unresolved (fromEvents)
import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as SP

main :: IO ()
main =
  -- request <- parseRequest "http://feeds.feedburner.com/seriouseats/recipes"
  -- doc <- runResourceT $ go request
  -- print doc
  createRequest >>= (\request
    -> runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ transformToDocument $ responseBody response
      )))) >>= \doc -> print doc

transformToDocument :: MonadThrow m => ConduitM i B.ByteString m () -> ConduitM i o m XT.Document
transformToDocument input = input .| SP.parseBytes SP.def .| CL.map (\e -> (Nothing, e)) .| fromEvents

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: IO Request
createRequest = parseRequest "http://feeds.feedburner.com/seriouseats/recipes"

-- go :: Request -> ResourceT IO Document
-- go request = do
--   manager <- lift (newManager tlsManagerSettings)
--   r <- (http request manager)
--   runConduit $ responseBody r .| parseBytes def .| CL.map (\e -> (Nothing, e)) .| fromEvents


module Main where
import Conduit (runConduit, (.|))
import Data.Conduit.List (sinkNull)
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit (http, newManager, tlsManagerSettings, Manager, parseRequest, Request, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Text.XML.Unresolved (fromEvents)
import qualified Text.XML.Stream.Parse as SP

main :: IO ()
main =
  -- request <- parseRequest "http://feeds.feedburner.com/seriouseats/recipes"
  -- doc <- runResourceT $ go request
  -- print doc
  createRequest >>= (\request
    -> runResourceT (getManager >>= (\manager ->
      (http request manager) >>= (\response ->
        runConduit $ responseBody response .| SP.parseBytes SP.def .| CL.map (\e -> (Nothing, e)) .| fromEvents
      )))) >>= \doc -> print doc

getManager :: ResourceT IO Manager
getManager = lift $ newManager tlsManagerSettings

createRequest :: IO Request
createRequest = parseRequest "http://feeds.feedburner.com/seriouseats/recipes"

-- go :: Request -> ResourceT IO Document
-- go request = do
--   manager <- lift (newManager tlsManagerSettings)
--   r <- (http request manager)
--   runConduit $ responseBody r .| parseBytes def .| CL.map (\e -> (Nothing, e)) .| fromEvents


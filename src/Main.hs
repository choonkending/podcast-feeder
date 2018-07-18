module Main where
import Conduit (runConduit, (.|))
import Data.Conduit.List (sinkNull)
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Text.XML
import Text.XML.Stream.Parse

main :: IO ()
main = do
  -- parseRequest "http://feeds.feedburner.com/seriouseats/recipes" >>=
  --   \req -> newManager tlsManagerSettings >>=
  --       \m -> http req m >>=
  --         \res -> putStrLn show (responseBody res)
  --       runConduit $ responseBody r will not work as the types do not match
  --       We need to compose it with a function that accepts an input of bytestring and output of void
  request <- parseRequest "http://feeds.feedburner.com/seriouseats/recipes"
  doc <- runResourceT $ go request
  print doc

go :: Request -> ResourceT IO Document
go request = do
  manager <- lift (newManager tlsManagerSettings)
  r <- (http request manager)
  runConduit $ responseBody r .| parseBytes def .| CL.map (\e -> (Nothing, e)) .| fromEvents


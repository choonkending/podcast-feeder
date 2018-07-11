module Main where
import Conduit (runConduit, (.|))
import Data.Conduit.List (sinkNull)
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
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
  manager <- newManager tlsManagerSettings
  doc <- runResourceT go
  print doc

go :: ResourceT IO Document
go = do
  r <- (http request manager)
  runConduit $ responseBody r .| parseBytes def .| fromEvents


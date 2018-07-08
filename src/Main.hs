module Main where
import Network.HTTP.Conduit

main :: IO ()
main = do
  response <- simpleHttp "http://feeds.feedburner.com/seriouseats/recipes"
  putStrLn (show response)

module Main where
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource

main :: IO ()
main = do
  -- parseRequest "http://feeds.feedburner.com/seriouseats/recipes" >>=
  --   \req -> newManager tlsManagerSettings >>=
  --       \m -> http req m >>=
  --         \res -> putStrLn show (responseBody res)
  request <- parseRequest "http://feeds.feedburner.com/seriouseats/recipes"
  manager <- newManager tlsManagerSettings
  response <- http request manager
  putStrLn (show response)



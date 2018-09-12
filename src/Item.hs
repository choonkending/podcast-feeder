{-# LANGUAGE DeriveGeneric #-}
module Item where
import qualified Data.Text as T
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)

data Item = Item {
  title :: T.Text,
  description :: T.Text,
  enclosure :: Enclosure
} deriving (Generic, Show)
instance ToJSON Item

data Enclosure = Enclosure {
  url :: T.Text,
  length :: Integer,
  mediaType :: T.Text
} deriving (Generic, Show)
instance ToJSON Enclosure


{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module Action where
import qualified Data.Text as T
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)

type UserID = T.Text
type Url = T.Text

data Progress = Progress {
  position :: Int,
  url :: Url
} deriving (Generic, Show)
instance ToJSON Progress

data Action a where
  StorePosition :: UserID -> Progress -> Action ()
  FetchPosition :: UserID -> Url -> Action (Maybe Progress)

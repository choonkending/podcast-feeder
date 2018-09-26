{-# LANGUAGE GADTs #-}
module Action where
import qualified Data.Text as T

type UserID = T.Text
type Url = T.Text

data Progress = Progress {
  position :: Int,
  url :: Url
}

data Action a where
  StorePosition :: UserID -> Progress -> Action ()
  FetchPosition :: UserID -> Url -> Action (Maybe Progress)

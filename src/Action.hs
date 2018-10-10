{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module Action where
import qualified Data.Text as T
import Data.Aeson (ToJSON(..))
import GHC.Generics (Generic)
import Control.Monad.Free (Free, liftF)

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
  -- GADT is creates a data constructor from the type signature
  Action :: (a -> b) -> Action a -> Action b

-- We needed to define it in Action data type because we cannot constrain b. b needs to be any type.
-- With only the StorePosition and FetchPosition, we would have constrained b to () or Maybe Progress

instance Functor Action where
  -- fmap f action@(StorePosition _ _) = Action f action -- f is a function from () to b
  -- fmap f action@(FetchPosition _ _) = Action f action
  -- fmap f action@(Action _ _) = Action f action
  -- fmap f (Action g action) = Action (f . g) action
  fmap = Action


-- g is a function from aa -> ab
-- action is Action aa
-- f is a function from fa -> fb
-- fa ~ ab
-- result needs to be Action fb
-- f . g is a function from aa -> fb

storePosition :: UserID -> Progress -> Free Action ()
storePosition userID progress = liftF (StorePosition userID progress)

fetchPosition :: UserID -> Url -> Free Action (Maybe Progress)
fetchPosition userID url = liftF (FetchPosition userID url)


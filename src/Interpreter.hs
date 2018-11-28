{-# LANGUAGE GADTs #-}
module Interpreter where
import qualified Action as A
import Data.List (find)

data Store = Store [UserStore]
data UserStore = UserStore A.UserID [A.Progress]

newStore :: Store
newStore = Store []

findUser :: A.UserID -> Store -> Maybe UserStore
findUser uid (Store listOfUserStores) = find (isUser uid) listOfUserStores

isUser :: A.UserID -> UserStore -> Bool
isUser givenUid (UserStore uid _) = givenUid == uid

findProgress :: A.Url -> UserStore -> Maybe A.Progress
findProgress url (UserStore _ progresses) = find (isProgress url) progresses

isProgress :: A.Url -> A.Progress -> Bool
isProgress givenUrl (A.Progress { A.url = url }) = givenUrl == url

storeProgress :: A.Progress -> UserStore -> UserStore
-- We could remove the duplicate old progress but findProgress only matches on the first progress, so that's a perf optimisation!
storeProgress progress (UserStore uid progresses) = UserStore uid (progress : progresses)

updateUserStore :: A.UserID -> (UserStore -> UserStore) -> Store -> Store
updateUserStore uid f store = Store (findUser uid : userStores)

interpret :: Store -> A.Action a -> IO a
interpret store (A.StorePosition uid progress) = pure (storeProgress progress <$> findUser uid store)
interpret store (A.FetchPosition uid url) = pure (findUser uid store >>= findProgress url)
interpret store (A.Action fa2b aa) = fa2b <$> interpret store aa

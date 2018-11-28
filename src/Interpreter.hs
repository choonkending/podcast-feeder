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

interpret :: Store -> A.Action a -> IO a
interpret store (A.StorePosition uid progress) = pure ()
interpret store (A.FetchPosition uid url) = pure (findUser uid store >>= findProgress url)
interpret store (A.Action fa2b aa) = fa2b <$> interpret store aa

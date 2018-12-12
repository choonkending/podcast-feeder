{-# LANGUAGE GADTs #-}
module Interpreter where
import qualified Action as A
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import Data.List (find)

data Store = Store [UserStore]
data UserStore = UserStore A.UserID [A.Progress]

newStore :: STM (TVar Store)
newStore = newTVar (Store [])

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

storePosition :: TVar Store -> A.UserID -> A.Progress -> STM ()
storePosition tvarStore uid progress = modifyTVar' tvarStore (updateUserStore uid (storeProgress progress))

interpret :: TVar Store -> A.Action a -> IO a
interpret tvarStore (A.StorePosition uid progress) = atomically (storePosition tvarStore uid progress)
interpret store (A.FetchPosition uid url) = pure (findUser uid store >>= findProgress url)
interpret store (A.Action fa2b aa) = fa2b <$> interpret store aa

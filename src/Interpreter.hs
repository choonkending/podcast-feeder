{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Interpreter where
import qualified Action as A
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import Data.List (find)

data Database = Database [UserStore]
data UserStore = UserStore A.UserID [A.Progress]
type Interpreter = (forall a. A.Action a -> IO a)

newDatabase :: STM (TVar Database)
newDatabase = newTVar (Database [])

findUser :: A.UserID -> Database -> Maybe UserStore
findUser uid (Database listOfUserStores) = find (isUser uid) listOfUserStores

isUser :: A.UserID -> UserStore -> Bool
isUser givenUid (UserStore uid _) = givenUid == uid

findProgress :: A.Url -> UserStore -> Maybe A.Progress
findProgress url (UserStore _ progresses) = find (isProgress url) progresses

isProgress :: A.Url -> A.Progress -> Bool
isProgress givenUrl (A.Progress { A.url = url }) = givenUrl == url

storeProgress :: A.Progress -> UserStore -> UserStore
-- We could remove the duplicate old progress but findProgress only matches on the first progress, so that's a perf optimisation!
storeProgress progress (UserStore uid progresses) = UserStore uid (progress : progresses)

updateUserStore :: A.UserID -> (UserStore -> UserStore) -> Database -> Database
updateUserStore uid f database@(Database userstores) = let
    emptyuserstore = UserStore uid []
    olduserstore = maybe emptyuserstore id (findUser uid database)
    newuserstore = f olduserstore
    newuserstores = newuserstore : userstores
  in Database newuserstores
-- updateUserStore uid f store = Store (findUser uid : userStores)

storePosition :: TVar Database -> A.UserID -> A.Progress -> STM ()
storePosition tvarDatabase uid progress = modifyTVar' tvarDatabase (updateUserStore uid (storeProgress progress))

fetchPosition :: TVar Database -> A.UserID -> A.Url -> STM (Maybe A.Progress)
fetchPosition  tvarDatabase uid url = do
  database <- readTVar tvarDatabase
  pure (findUser uid database >>= findProgress url)

interpret :: TVar Database -> Interpreter
interpret tvarDatabase (A.StorePosition uid progress) = atomically (storePosition tvarDatabase uid progress)
-- interpret store (A.FetchPosition uid url) = pure (findUser uid store >>= findProgress url)
interpret tvarDatabase (A.FetchPosition uid url) = atomically (fetchPosition tvarDatabase uid url)
interpret store (A.Action fa2b aa) = fa2b <$> interpret store aa






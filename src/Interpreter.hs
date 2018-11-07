{-# LANGUAGE GADTs #-}
module Interpreter where
import qualified Action as A

interpret :: A.Action a -> IO a
interpret (A.StorePosition uid progress) = pure ()
interpret (A.FetchPosition uid url) = pure Nothing
interpret (A.Action fa2b aa) = fa2b <$> interpret aa

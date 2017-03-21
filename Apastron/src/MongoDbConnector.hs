{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MongoDbConnector where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Logger

connectToDatabase :: Action IO a -> IO a
connectToDatabase act = do
  pipe <- connect (host "127.0.0.1")
  access pipe master "ApastronDatabase" act

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)
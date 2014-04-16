{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

Models.Kata where

import Database.Persist.Postgresql (withPostgresqlConn)
import Web.Heroku (dbConnParams)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Post
    title Text
    content Text
    createdAt UTCTime
    deriving Show
|]

runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
    params <- dbConnParams
    let connStr = foldr (\(k, v) t ->
        t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
    runResourceT . withPostgresqlConn connStr $ runSqlConn query

readPosts :: IO [Entity Post]
readPosts = (runDb $ selectList [] [LimitTo 10])
 
blaze = S.html . renderHtml

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Hermes.Persistence.SQLite(persistSQLite) where

import qualified Control.Hermes.Persistence as H
import Control.Hermes.Persistence.Persistent
import qualified Control.Hermes.Types as H
import Control.Monad(join)
import Control.Monad.Catch(catch, SomeException)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(NoLoggingT)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.Resource(MonadUnliftIO, ResourceT)
import Data.Int(Int64)
import Data.List(nub)
import Data.Text(Text, pack, unpack)
import Data.UUID(UUID, toString)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Kind
    uid         H.KindUid
    eventIndex  Int64
    actions     [String]
    Primary uid eventIndex
    deriving Show
Subject
    kind H.KindUid
    uid  H.SubjectUid
    Primary kind uid
    deriving Show
Event
    kind    H.KindUid
    subject H.SubjectUid
    uid     H.EventUid
    index   Int64
    action  String
    content Text
    Primary kind subject uid
    deriving Show
|]

type SQLiteDBPath = Text
type SQLiteAction m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) a

persistSQLite :: SQLiteDBPath -> H.Configuration (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
persistSQLite = Configuration

instance H.Persist (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) where
  data Configuration (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) = Configuration { dbPath :: SQLiteDBPath }

  runPersistance db = runSqlite $ dbPath db

  initPersistence = runMigration migrateAll

  newKind kind = do
    flip catch catcher $ do
      insert_ $ Kind (H.kindUid kind) 0 (nub $ map H.extractAction $ H.kindActions kind)
      return H.KindCreated
    where catcher :: forall m. Monad m => SomeException -> m H.NewKindStatus
          catcher _ = return H.KindAlreadyExisting

  fetchKind uid = do
    fetched <- aggregateKindRecords uid
    return $ fmap (\(Kind u _ a) -> H.Kind u $ map H.Action a) fetched

  addAction uid (H.Action action) = do
    aggregatedKind  <- aggregateKindRecords uid
    case aggregatedKind of
      Nothing           -> return H.ActionKindNotExists
      Just (Kind _ i a) -> do
        if elem action a
          then return H.ActionAlreadyExisting
          else do
            insert_ $ Kind uid (i + 1) [action]
            return H.ActionAdded

  newSubject subject = do
    flip catch catcher $ do
      insert_ $ Subject (H.subjectKind subject) (H.subjectUid subject)
      return H.SubjectCreated
    where catcher :: forall m. Monad m => SomeException -> m H.NewSubjectStatus
          catcher _ = return H.SubjectAlreadyExisting

  fetchSubject kind uid = do
    fmap (fromEntity . entityVal) <$> selectFirst [SubjectKind ==. kind, SubjectUid ==. uid] []
    where fromEntity x = H.Subject (subjectKind x) (subjectUid x)

  listAllowedActions kind subject = do
    fetchedSubject <- H.fetchSubject kind subject
    fetchedKind <- sequence $ const (H.fetchKind kind)  <$> fetchedSubject
    return $ H.kindActions <$> join fetchedKind

  newEvent x = do
    event <- liftIO $ H.buildNewEvent x
    events <- H.listEvents (H.eventKind event) (H.eventSubject event)
    let nextEventIndex = fromIntegral $ length events
    insert_ $ Event (H.eventKind event) (H.eventSubject event) (H.eventUid event) nextEventIndex (H.extractAction $ H.eventAction event) (pack $ show $ H.extractEventData $ H.eventContent event)
    return $ H.eventUid event

  listEvents kind subject = do
    map fromEntity <$> fetchEvents
    where fetchEvents = map entityVal <$> selectList [EventKind ==. kind, EventSubject ==. subject] [Asc EventIndex]
          fromEntity event = H.Event (eventKind event) (eventSubject event) (eventUid event) (H.Action $ eventAction event) (H.EventData $ read $ unpack $ eventContent event)

-- Helpers

aggregateKindRecords :: H.KindUid -> SQLiteAction IO (Maybe Kind)
aggregateKindRecords uid = do
  records <- selectList [KindUid ==. uid] [Asc KindEventIndex]
  if null records
    then return Nothing
    else return $ Just (buildKind records)
  where buildKind = foldr1 (\(Kind _ _ x) (Kind _ i y) -> Kind uid i (x ++ y)) . map entityVal

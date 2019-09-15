{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Hermes.Persistence.SQLite(persistSQLite) where

import qualified Control.Hermes.Persistence as H
import Control.Hermes.Persistence.Persistent
import qualified Control.Hermes.Types as H
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
    uid H.KindUid
    eventIndex Int64
    actions [String]
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
    fetched <- aggregateKindRecords $ H.kindUid kind
    case fetched of
      Just _  -> return H.KindAlreadyExisting
      Nothing -> do
                  insert_ $ Kind (H.kindUid kind) 0 (nub $ map H.extractAction $ H.kindActions kind)
                  return H.KindCreated

  fetchKind uid = do
    fetched <- aggregateKindRecords uid
    return $ fmap (\(Kind u _ a) -> H.Kind u $ map H.Action a) fetched

  addAction uid (H.Action action) = do
    fetched <- aggregateKindRecords uid
    case fetched of
      Nothing -> return H.ActionKindDoesNotExists
      Just (Kind _ i a) -> if elem action a
                             then return H.ActionAlreadyExisting
                             else do
                               insert_ $ Kind uid (i + 1) [action]
                               return H.ActionAdded

  newSubject subject = do
    fetchedKind <- aggregateKindRecords $ H.subjectKind subject
    case fetchedKind of
      Nothing -> return H.SubjectKindDoesNotExists
      Just _ -> do
                  fetched <- selectFirst [SubjectKind ==. H.subjectKind subject, SubjectUid ==. H.subjectUid subject] []
                  case fetched of
                    Just _ -> return H.SubjectAlreadyExisting
                    Nothing -> do
                                insert_ $ Subject (H.subjectKind subject) (H.subjectUid subject)
                                return H.SubjectCreated

  newEvent x = do
    fetchedSubject <- selectFirst [SubjectKind ==. H.newEventKind x, SubjectUid ==. H.newEventSubject x] []
    case fetchedSubject of
      Nothing -> return H.EventSubjectDoesNotExists
      Just _ -> do
                  event <- liftIO $ H.buildNewEvent x
                  insert $ Event (H.eventKind event) (H.eventSubject event) (H.eventUid event) (H.extractAction $ H.eventAction event) (pack $ show $ H.extractEventData $ H.eventContent event)
                  return H.EventCreated

  listEvents kind subject = do
    fetchedSubject <- selectFirst [SubjectKind ==. kind, SubjectUid ==. subject] []
    sequence $ (const (map fromEntity <$> fetchEvents)) <$> fetchedSubject
    where fetchEvents = map entityVal <$> selectList [EventKind ==. kind, EventSubject ==. subject] [Asc EventId]
          fromEntity event = H.Event (eventKind event) (eventSubject event) (eventUid event) (H.Action $ eventAction event) (H.EventData $ read $ unpack $ eventContent event)

-- Helpers

aggregateKindRecords :: H.KindUid -> SQLiteAction IO (Maybe Kind)
aggregateKindRecords uid = do
  records <- selectList [KindUid ==. uid] [Asc KindEventIndex]
  if null records
    then return Nothing
    else return $ Just (buildKind records)
  where buildKind = foldr1 (\(Kind _ _ x) (Kind _ i y) -> Kind uid i (x ++ y)) . map entityVal

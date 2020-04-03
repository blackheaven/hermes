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
import Data.Functor(($>))
import Data.Int(Int64)
import Data.List(foldl', nub)
import qualified Data.Map.Lazy as M
import Data.Maybe(maybe, listToMaybe)
import qualified Data.Set as S
import Data.Text(Text, pack, unpack)
import Data.UUID(UUID, toString)
import Database.Persist
import Database.Persist.Sql(runMigrationSilent)
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
    subject H.Subject
    uid     H.EventUid
    index   Int64
    action  String
    content Text
    UniqueEventUid subject uid
    UniqueEventIndex subject index
    deriving Show
Subscription
    subscriber H.Subscriber
    subject    H.Subject
    index      Int64
    subscribe  Bool
    deriving Show
Viewing
    subscriber  H.Subscriber
    globalIndex EventId
    Primary subscriber
    deriving Show
|]

type SQLiteDBPath = Text
type SQLiteAction m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) a

persistSQLite :: SQLiteDBPath -> H.Configuration (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))
persistSQLite = Configuration

instance H.Persist (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) where
  data Configuration (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) = Configuration { dbPath :: SQLiteDBPath }

  runPersistance db = runSqlite $ dbPath db

  initPersistence = runMigrationSilent migrateAll >> return ()

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

  fetchSubject (H.Subject kind uid) = do
    fmap (fromEntity . entityVal) <$> selectFirst [SubjectKind ==. kind, SubjectUid ==. uid] []
    where fromEntity x = H.Subject (subjectKind x) (subjectUid x)

  listAllowedActions subject@(H.Subject kind _) = do
    fetchedSubject <- H.fetchSubject subject
    fetchedKind <- sequence $ const (H.fetchKind kind)  <$> fetchedSubject
    return $ H.kindActions <$> join fetchedKind

  newEvent x = do
    event <- liftIO $ H.buildNewEvent x
    events <- H.listEvents (H.eventSubject event)
    let nextEventIndex = fromIntegral $ length events
    id <- insert $ Event (H.eventSubject event) (H.eventUid event) nextEventIndex (H.extractAction $ H.eventAction event) (pack $ show $ H.extractEventData $ H.eventContent event)
    return $ H.eventUid event

  listEvents subject = do
    map eventFromEntity <$> fetchEvents
    where fetchEvents = map entityVal <$> selectList [EventSubject ==. subject] [Asc EventIndex]

  subscribe  (H.Subscription subscriber subject) = do
    event <- lastEvent subject
    let perform e = insertSubscription subscriber subject e True $> eventUid e
    sequence $ perform <$> event

  unsubscribe  (H.Subscription subscriber subject) = do
    event <- lastEvent subject
    previousSubscriptionEvent <- fmap entityVal <$> selectFirst [SubscriptionSubscriber ==. subscriber, SubscriptionSubject ==. subject] [Desc SubscriptionIndex]
    let previousSubscription = previousSubscriptionEvent >>= \e -> if subscriptionSubscribe e then Just e else Nothing
    sequence $ flip fmap (previousSubscription >> event) $ (\e -> insertSubscription subscriber subject e False $> eventUid e)

  listNotifiations subscriber = do
    lastViewing <- fmap (viewingGlobalIndex . entityVal) <$> selectFirst [ViewingSubscriber ==. subscriber] []
    subscriptionsEvents <- fmap entityVal <$> selectList [SubscriptionSubscriber ==. subscriber] [Asc SubscriptionIndex]
    let subscriptions = listSubscriptions subscriptionsEvents
    let newEventsSinceLastTime l = selectList [EventId >. l, EventSubject <-. map fst subscriptions] [Asc EventId]
    let newEventsFromBeginning = selectList [EventSubject <-. map fst subscriptions] [Asc EventId]
    let subscriptionMap = M.fromList subscriptions
    let takeSinceSubscription e = eventIndex e > subscriptionMap M.! eventSubject e
    (map eventFromEntity . filter takeSinceSubscription . map entityVal) <$> maybe newEventsFromBeginning newEventsSinceLastTime lastViewing

  viewNotifiations subscriber = do
    lastEvent <- listToMaybe <$> selectKeysList [] [LimitTo 1, Desc EventId]
    let fetchViewingKey = listToMaybe <$> selectKeysList [ViewingSubscriber ==. subscriber] [LimitTo 1]
    let perform r = fetchViewingKey >>= maybe (insert_ r) (flip replace r)
    sequence_ $ (perform . Viewing subscriber) <$> lastEvent

-- Helpers
aggregateKindRecords :: H.KindUid -> SQLiteAction IO (Maybe Kind)
aggregateKindRecords uid = do
  records <- selectList [KindUid ==. uid] [Asc KindEventIndex]
  if null records
    then return Nothing
    else return $ Just (buildKind records)
  where buildKind = foldr1 (\(Kind _ _ x) (Kind _ i y) -> Kind uid i (x ++ y)) . map entityVal

lastEvent :: H.Subject -> SQLiteAction IO (Maybe Event)
lastEvent subject =
  fmap entityVal <$> selectFirst [EventSubject ==. subject] [Desc EventIndex]

insertSubscription :: H.Subscriber -> H.Subject -> Event -> Bool -> SQLiteAction IO ()
insertSubscription subscriber subject event subscribe =
  insert_ $ Subscription subscriber subject (eventIndex event) subscribe

eventFromEntity :: Event -> H.Event
eventFromEntity event = H.Event (eventSubject event) (eventUid event) (H.Action $ eventAction event) (H.EventData $ read $ unpack $ eventContent event)

listSubscriptions :: [Subscription] -> [(H.Subject, Int64)]
listSubscriptions = map getSubscriptionResult . S.toList . foldl' go S.empty
  where go xs x = let result = (SubscriptionResult (subscriptionSubject x, subscriptionIndex x))
                    in if subscriptionSubscribe x
                        then S.insert result xs
                        else S.delete result xs

newtype SubscriptionResult = SubscriptionResult { getSubscriptionResult :: (H.Subject, Int64) }

instance Eq SubscriptionResult where
  SubscriptionResult (x, _) == SubscriptionResult (y, _) = x == y

instance Ord SubscriptionResult where
  SubscriptionResult (x, _) `compare` SubscriptionResult (y, _) = x `compare` y

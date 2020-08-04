{-# LANGUAGE DeriveGeneric #-}

module Control.Hermes.Types(
                             KindUid(..)
                           , Kind(..)
                           , SubjectUid(..)
                           , Subject(..)
                           , Action(..)
                           , EventUid(..)
                           , EventData(..)
                           , Event(..)
                           , Subscriber(..)
                           , Subscription(..)
                           , Notification
                           , extractAction
                           , extractEventData
                           , extractEventUid
                           , extractSubjectUid
                           , extractKindUid
                           ) where

import Data.Aeson.Types(Value)
import Data.UUID(UUID)
import GHC.Generics

newtype KindUid = KindUid String deriving (Eq, Generic, Ord, Read, Show)

extractKindUid :: KindUid -> String
extractKindUid (KindUid x) = x

newtype Action = Action String deriving (Eq, Generic, Ord, Read, Show)

extractAction :: Action -> String
extractAction (Action x) = x

data Kind = Kind {
             kindUid     :: KindUid
           , kindActions :: [Action]
           } deriving (Eq, Generic, Ord, Read, Show)

newtype SubjectUid = SubjectUid String deriving (Eq, Generic, Ord, Read, Show)

extractSubjectUid :: SubjectUid -> String
extractSubjectUid (SubjectUid x) = x

data Subject = Subject {
               subjectKind :: KindUid
             , subjectUid  :: SubjectUid
             } deriving (Eq, Generic, Ord, Read, Show)

newtype EventUid = EventUid UUID deriving (Eq, Generic, Ord, Read, Show)

extractEventUid :: EventUid -> UUID
extractEventUid (EventUid x) = x

newtype EventData = EventData Value deriving (Eq, Generic, Read, Show)

extractEventData :: EventData -> Value
extractEventData (EventData x) = x

instance Ord EventData where
  compare (EventData x) (EventData y) = show x `compare` show y

data Event = Event {
             eventSubject :: Subject
           , eventUid     :: EventUid
           , eventAction  :: Action
           , eventContent :: EventData
           } deriving (Eq, Generic, Ord, Read, Show)

newtype Subscriber = Subscriber String deriving (Eq, Generic, Ord, Read, Show)

data Subscription = Subscription {
                    subscriber :: Subscriber
                  , subject    :: Subject
                  } deriving (Eq, Generic, Ord, Read, Show)

type Notification = Event

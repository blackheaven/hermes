{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Hermes.Types(
                             KindUid(..)
                           , Kind(..)
                           , SubjectUid(..)
                           , Subject(..)
                           , Action(..)
                           , EventUid(..)
                           , EventData(..)
                           , Event(..)
                           , extractAction
                           , extractEventData
                           ) where

import Data.Aeson.Types(Value)
import Data.UUID(UUID)
import GHC.Generics

newtype KindUid = KindUid String deriving (Eq, Generic, Ord, Read, Show)
newtype Action = Action String deriving (Eq, Generic, Ord, Read, Show)

extractAction :: Action -> String
extractAction (Action x) = x

data Kind = Kind {
             kindUid     :: KindUid
           , kindActions :: [Action]
           } deriving (Eq, Generic, Ord, Read, Show)

newtype SubjectUid = SubjectUid String deriving (Eq, Generic, Ord, Read, Show)

data Subject = Subject {
               subjectKind :: KindUid
             , subjectUid  :: SubjectUid
             } deriving (Eq, Generic, Ord, Read, Show)

newtype EventUid = EventUid UUID deriving (Eq, Generic, Ord, Read, Show)
newtype EventData = EventData Value deriving (Eq, Generic, Read, Show)

extractEventData :: EventData -> Value
extractEventData (EventData x) = x

instance Ord EventData where
  compare (EventData x) (EventData y) = show x `compare` show y

data Event = Event {
             eventKind    :: KindUid
           , eventSubject :: SubjectUid
           , eventUid     :: EventUid
           , eventAction  :: Action
           , eventContent :: EventData
           } deriving (Eq, Generic, Ord, Read, Show)
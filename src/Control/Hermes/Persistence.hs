{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Hermes.Persistence(
                                   Persist(..)
                                 , NewKindStatus(..)
                                 , NewActionStatus(..)
                                 , NewSubjectStatus(..)
                                 , NewEvent(..)
                                 , buildNewEvent
                                 ) where

import Control.Hermes.Types
import GHC.Generics
import Codec.Binary.UTF8.String(encode)
import Data.List(concat, cycle)
import Data.Maybe (fromJust)
import Data.UUID(UUID, fromString, toString)
import Data.UUID.V4(nextRandom)
import Data.UUID.V5(generateNamed)

class Monad p => Persist p where
  data Configuration p :: *
  runPersistance       :: Configuration p -> p a -> IO a
  initPersistence      :: p ()
  newKind              :: Kind -> p NewKindStatus
  fetchKind            :: KindUid -> p (Maybe Kind)
  addAction            :: KindUid -> Action -> p NewActionStatus
  newSubject           :: Subject -> p NewSubjectStatus
  fetchSubject         :: Subject -> p (Maybe Subject)
  listAllowedActions   :: Subject -> p (Maybe [Action])
  newEvent             :: NewEvent -> p EventUid
  listEvents           :: Subject -> p [Event]
  subscribe            :: Subscription -> p (Maybe EventUid)
  unsubscribe          :: Subscription -> p (Maybe EventUid)
  listNotifiations     :: Subscriber -> p [Notification]
  viewNotifiations     :: Subscriber -> p ()

data NewEvent = NewEvent {
                newEventSubject :: Subject
              , newEventAction  :: Action
              , newEventContent :: EventData
              } deriving (Show, Eq, Generic)

data NewKindStatus =
                     KindCreated
                   | KindAlreadyExisting
                   deriving (Show, Eq)

data NewActionStatus =
                       ActionAdded
                     | ActionAlreadyExisting
                     | ActionKindNotExists
                   deriving (Show, Eq)

data NewSubjectStatus =
                        SubjectCreated
                      | SubjectAlreadyExisting
                   deriving (Show, Eq)

makeUUID :: NewEvent -> IO UUID
makeUUID e = do
  randomUUID <- cycle . toString <$> nextRandom
  let salted = concat $ zipWith (\x y -> [x, y]) (show e) randomUUID
  return $ generateNamed eventOID (encode salted)

buildNewEvent :: NewEvent -> IO Event
buildNewEvent e = do
  uuid <- makeUUID e
  return $ Event (newEventSubject e) (EventUid uuid) (newEventAction e) (newEventContent e)

eventOID :: UUID
eventOID = fromJust $ fromString "11e1bcce-3e11-ec11-ce11-e3eccb1e1111"

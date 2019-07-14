{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import Servant.API

type HermesAPI =
       KindEndpoint
  :<|> SubjectEndpoint
  :<|> EventEndpoint

hermesAPI :: Proxy HermesAPI
hermesAPI = Proxy

type KindPrefixer a =
  "kinds" :> Capture "kind" KindUid :> a
type KindEndpoint =
  KindPrefixer (ReqBody '[JSON] KindBody :> Post '[JSON] ())

type SubjectPrefixer a =
  KindPrefixer ("subjects" :> Capture "subject" SubjectUid :> a)
type SubjectEndpoint =
  SubjectPrefixer (ReqBody '[JSON] EventData :> Post '[JSON] NewEvent)

type EventPrefixer a =
  SubjectPrefixer ("events" :> a)
type EventEndpoint =
       EventPrefixer (Capture "action" Action :> ReqBody '[JSON] EventData :> Post '[JSON] NewEvent)
  :<|> EventPrefixer (Get  '[JSON] [Event])

newtype Action = Action String deriving Generic
instance ToJSON Action
instance FromJSON Action
instance FromHttpApiData Action where
  parseUrlPiece = fmap Action . parseUrlPiece
data KindBody = KindBody [Action] deriving Generic
instance ToJSON KindBody
instance FromJSON KindBody

newtype KindUid = KindUid String deriving Generic
instance ToJSON KindUid
instance FromHttpApiData KindUid where
  parseUrlPiece = fmap KindUid . parseUrlPiece
newtype SubjectUid = SubjectUid String deriving Generic
instance ToJSON SubjectUid
instance FromHttpApiData SubjectUid where
  parseUrlPiece = fmap SubjectUid . parseUrlPiece
newtype EventUid = EventUid String deriving Generic
instance ToJSON EventUid
newtype NewEvent = NewEvent { event_uid :: EventUid } deriving Generic
instance ToJSON NewEvent
newtype EventData = EventData Value deriving Generic
instance ToJSON EventData
instance FromJSON EventData

data Event = Event {
             uid     :: KindUid
           , action  :: Action
           , content :: EventData
           } deriving Generic
instance ToJSON Event

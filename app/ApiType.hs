{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import qualified Control.Hermes.Types as T
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import Servant.API

type HermesAPI =
       KindEndpoint
  :<|> SubjectEndpoint
  :<|> EventEndpoint
  :<|> SubscriberEndpoint

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

type SubscriberPrefixer a =
  "subscribers" :> a
type SubscriberEndpoint =
       SubscriberPrefixer (Capture "subscriber" Subscriber :> ReqBody '[JSON] Subscription :> Post '[JSON] ())
  :<|> SubscriberPrefixer (Capture "subscriber" Subscriber :> ReqBody '[JSON] Subscription :> Delete '[JSON] ())
  :<|> SubscriberPrefixer (Capture "subscriber" Subscriber :> QueryFlag "view" :> Get  '[JSON] [Notification])

newtype Action = Action String deriving Generic
instance ToJSON Action
instance FromJSON Action
instance FromHttpApiData Action where
  parseUrlPiece = fmap Action . parseUrlPiece

convertAction :: Action -> T.Action
convertAction (Action x) = T.Action x

newtype Subscriber = Subscriber String deriving Generic
instance ToJSON Subscriber
instance FromJSON Subscriber
instance FromHttpApiData Subscriber where
  parseUrlPiece = fmap Subscriber . parseUrlPiece

data KindBody = KindBody [Action] deriving Generic
instance ToJSON KindBody
instance FromJSON KindBody

newtype KindUid = KindUid String deriving Generic
instance ToJSON KindUid
instance FromJSON KindUid
instance FromHttpApiData KindUid where
  parseUrlPiece = fmap KindUid . parseUrlPiece
newtype SubjectUid = SubjectUid String deriving Generic
instance ToJSON SubjectUid
instance FromJSON SubjectUid
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
             uid     :: EventUid
           , action  :: Action
           , content :: EventData
           } deriving Generic
instance ToJSON Event

data Subscription = Subscription KindUid SubjectUid

instance FromJSON Subscription where
    parseJSON = withObject "Subscription" $ \x -> Subscription
        <$> x .: "kind"
        <*> x .: "subject"

data Notification = Notification KindUid SubjectUid EventUid Action EventData

instance ToJSON Notification where
  toJSON (Notification k s u a d) = object ["kind" .= k, "subject" .= s, "uid" .= u, "action" .= a, "data" .= d]

  toEncoding (Notification k s u a d) = pairs ("kind" .= k <> "subject" .= s <> "uid" .= u <> "action" .= a <> "data" .= d)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Hermes.RabbitMQ(
                                pushedNotifiations
                              , wrapPersistenceRabbitMQ
                              ) where

import Data.Aeson(decode, encode)
import qualified Control.Hermes.Persistence as H
import Control.Hermes.Types
import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Reader(ReaderT, ask, runReaderT)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)
import qualified Data.Text as T
import Data.Text.Lazy(toStrict)
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import Data.UUID(fromString, toString)
import Network.AMQP
import Network.AMQP.Streamly
import Network.AMQP.Types
import qualified Streamly as S
import qualified Streamly.Prelude as S

pushedNotifiations :: Channel -> Subscriber -> S.SerialT IO Notification
pushedNotifiations channel subscriber = do
  (queueName, _, _) <- liftIO $ declareQueue channel newQueue { queueExclusive = True }
  liftIO $ bindQueue channel queueName (subscriberExchange subscriber) noRoutingKey
  S.mapMaybe (deserialize . fst) $ consume channel queueName NoAck

wrapPersistenceRabbitMQ :: H.Persist p => Channel -> H.Configuration p -> H.Configuration (ReaderT Channel p)
wrapPersistenceRabbitMQ = Configuration

instance (H.Persist p, MonadIO p) => H.Persist (ReaderT Channel p) where
  data Configuration (ReaderT Channel p) = Configuration Channel (H.Configuration p)

  runPersistance (Configuration channel persistence) = H.runPersistance persistence . ($ channel) . runReaderT

  initPersistence = lift H.initPersistence

  newKind kind = lift $ H.newKind kind
  fetchKind uid = lift $ H.fetchKind uid
  addAction uid action = lift $ H.addAction uid action
  newSubject subject = do
    result <- lift $ H.newSubject subject
    channel <- ask
    liftIO $ declareExchange channel newExchange { exchangeName = subjectExchange subject, exchangeType = "direct" }
    return result

  fetchSubject subject = lift $ H.fetchSubject subject
  listAllowedActions subject = lift $ H.listAllowedActions subject

  newEvent x = do
    uuid <- lift $ H.newEvent x
    let e = Event (H.newEventSubject x) uuid (H.newEventAction x) (H.newEventContent x)
    channel <- ask
    liftIO $ publishMsg' channel (subjectExchange $ H.newEventSubject x) noRoutingKey False $ serialize e
    return uuid

  listEvents subject = lift $ H.listEvents subject

  subscribe subscription@(Subscription subscriber subject) = do
    result <- lift $ H.subscribe subscription
    channel <- ask
    when (isJust result) $ liftIO $ do
      declareExchange channel newExchange { exchangeName = subscriberExchange subscriber, exchangeType = "fanout" }
      bindExchange channel (subscriberExchange subscriber) (subjectExchange subject) noRoutingKey
    return result

  unsubscribe subscription@(Subscription subscriber subject) = do
    result <- lift $ H.unsubscribe subscription
    channel <- ask
    when (isJust result) $ liftIO $ unbindExchange channel (subscriberExchange subscriber) (subjectExchange subject) noRoutingKey
    return result


  listNotifiations subscriber = lift $ H.listNotifiations subscriber
  viewNotifiations subscriber = lift $ H.viewNotifiations subscriber

-- Helpers
subscriberExchange :: Subscriber -> T.Text
subscriberExchange (Subscriber subscriber) = T.pack $ "subscriber." <> subscriber

subjectExchange :: Subject -> T.Text
subjectExchange (Subject (KindUid kind) (SubjectUid subject)) = T.pack $ "subject." <> kind <> "." <> subject

serialize :: Notification -> Message
serialize e = newMsg { msgBody = body, msgHeaders = Just fieldTable }
  where body = encode $ extractEventData $ eventContent e
        fieldTable = FieldTable $ M.fromList $ fmap (FVString . encodeUtf8) <$> headers
        headers = [ (kindHN, T.pack $ extractKindUid $ subjectKind $ eventSubject e)
                  , (subjectHN, T.pack $ extractSubjectUid $ subjectUid $ eventSubject e)
                  , (uidHN, T.pack $ toString $ extractEventUid $ eventUid e)
                  , (actionHN, T.pack $ extractAction $ eventAction e)]

deserialize :: Message -> Maybe Notification
deserialize m = msgHeaders m >>= \(FieldTable headers) -> do
  let extractString = \case
                        FVString x -> Just $ T.unpack $ decodeUtf8 x
                        _          -> Nothing
  let fetchString f = headers M.!? f >>= extractString
  kindUid <- KindUid <$> fetchString kindHN
  subjectUid <- SubjectUid <$> fetchString subjectHN
  let subject = Subject kindUid subjectUid
  uid <- EventUid <$> (fetchString uidHN >>= fromString)
  action <- Action <$> fetchString actionHN
  Event subject uid action . EventData <$> decode (msgBody m)

kindHN, subjectHN, uidHN, actionHN :: T.Text
(kindHN, subjectHN, uidHN, actionHN) = ("kind", "subject", "uid", "action")

noRoutingKey :: T.Text
noRoutingKey = ""

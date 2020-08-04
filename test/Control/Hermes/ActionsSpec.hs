module Control.Hermes.ActionsSpec (main, spec) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Hermes.Actions
import Control.Hermes.Persistence(Configuration, NewEvent(..), Persist, initPersistence, runPersistance)
import Control.Hermes.Persistence.SQLite(persistSQLite)
import Control.Hermes.RabbitMQ
import Control.Hermes.RabbitMQUtils
import Control.Hermes.Types
import           Control.Monad.IO.Class         ( liftIO )
import Data.Aeson(Value(Bool))
import Data.Function(on)
import Data.Maybe(fromJust)
import Data.Text(pack)
import Data.UUID(fromString)
import Network.AMQP
import Test.Hspec
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

main :: IO ()
main = hspec spec

law :: Persist p => (a -> Configuration p) -> SpecWith a
law configuration = do
  describe "createKind" $ do
    it "creating should work" $ \arg ->
      assert (Right ()) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
    it "creating a kind twice should return an error" $ \arg ->
      assert (Right (), Left KindAlreadyExistingError) $ runPersistance (configuration arg) $ do
        initPersistence
        createKindResult1 <- createKind kindBase
        createKindResult2 <- createKind kindBase
        return (createKindResult1, createKindResult2)
  describe "addAction" $ do
    it "creating then adding an action should work" $ \arg ->
      assert (Right (), Right ()) $ runPersistance (configuration arg) $ do
        initPersistence
        createKindResult <- createKind kindBase
        addActionResult <- addAction kindBaseUid actionAdditional
        return (createKindResult, addActionResult)
    it "creating then adding an action twice should fail the second time" $ \arg ->
      assert (Right (), Right (), Left ActionAlreadyExistingError) $ runPersistance (configuration arg) $ do
        initPersistence
        createKindResult <- createKind kindBase
        addActionResult1 <- addAction kindBaseUid actionAdditional
        addActionResult2 <- addAction kindBaseUid actionAdditional
        return (createKindResult, addActionResult1, addActionResult2)
    it "adding an action on an unknown kind should fail" $ \arg ->
      assert (Left ActionKindDoesNotExistsError) $ runPersistance (configuration arg) $ do
        initPersistence
        addAction kindBaseUid actionAdditional
  describe "createSubject" $ do
    it "creating with a non existing kind should return an error" $ \arg ->
      assert (Left SubjectKindDoesNotExistsError) $ runPersistance (configuration arg) $ do
        initPersistence
        createSubject subjectBase subjectBaseEventData
    it "creating with an existing kind should be ok" $ \arg ->
      assert (Right (), Right eventBaseUUIDNeutral) $ runPersistance (configuration arg) $ do
        initPersistence
        createKindResult <- createKind kindBase
        createSubjectResult <- createSubject subjectBase subjectBaseEventData
        return (createKindResult, stabilizeEventUidResult createSubjectResult)
    it "creating a subject twice should return an error" $ \arg ->
      assert (Right (), Right eventBaseUUIDNeutral, Left SubjectAlreadyExistingError) $ runPersistance (configuration arg) $ do
        initPersistence
        createKindResult <- createKind kindBase
        createSubjectResult1 <- createSubject subjectBase subjectBaseEventData
        createSubjectResult2 <- createSubject subjectBase subjectBaseEventData
        return (createKindResult, stabilizeEventUidResult createSubjectResult1, stabilizeEventUidResult createSubjectResult2)
  describe "Event" $ do
    it "creating with a non existing subject should return an error" $ \arg ->
      assert (Left EventSubjectDoesNotExistsError, Nothing) $ runPersistance (configuration arg) $ do
        initPersistence
        newEventResult <- addEvent newEventBase1
        events <- listEvents subjectBase
        return (newEventResult, events)
    it "creating with an unknown action should return an error" $ \arg ->
      assert (Left EventActionNotAllowedError, Just [eventBaseInit]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEventResult <- addEvent newEventAdditionalInformation
        events <- listEvents subjectBase
        return (addEventResult, map stabilizeEventUid <$> events)
    it "creating with an existing subject should listed" $ \arg ->
      assert (Just [eventBaseInit, eventBase1]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        events <- listEvents subjectBase
        return $ map stabilizeEventUid <$> events
    it "creating two events should listed" $ \arg ->
      assert (Just [eventBaseInit, eventBase1, eventBase2]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        newEventResult1 <- addEvent newEventBase1
        newEventResult2 <- addEvent newEventBase2
        events <- listEvents subjectBase
        return $ map stabilizeEventUid <$> events
    it "creating an vent event twice should listed" $ \arg ->
      assert (Just True, Just [eventBaseInit, eventBase1, eventBase1]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        addEvent newEventBase1
        events <- listEvents subjectBase
        let eventsUidDistinction e = all id $ zipWith ((/=) `on` eventUid) e (tail e)
        return (eventsUidDistinction <$> events, map stabilizeEventUid <$> events)
  describe "Subscription" $ do
    it "creating with an existing subject should be ok" $ \arg ->
      assert True $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
    it "creating with an unknown subject should be empty" $ \arg ->
      assert False $ runPersistance (configuration arg) $ do
        initPersistence
        subscribe subscriptionBase
    it "deleting an existing subscription should be ok" $ \arg ->
      assert (True, True) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        action1 <- subscribe subscriptionBase
        action2 <- unsubscribe subscriptionBase
        return (action1, action2)
    it "deleting an unknown subscription should be empty" $ \arg ->
      assert False $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        unsubscribe subscriptionBase
  describe "Notification" $ do
    it "listing an unknown subscriber should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        viewNotifiations subscriberBase True
    it "listing with a new subscription should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        viewNotifiations subscriberBase True
    it "listing with a deleted subscription and an event should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        unsubscribe subscriptionBase
        addEvent newEventBase3
        viewNotifiations subscriberBase True
    it "listing with subscriptions and events should be filled" $ \arg ->
      assert [eventBase2, eventBase3] $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        addEvent newEventBase3
        map stabilizeEventUid <$> viewNotifiations subscriberBase True
    it "listing with a subscription deleted and recreated and events should be filled with the last events" $ \arg ->
      assert [eventBase3] $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        unsubscribe subscriptionBase
        subscribe subscriptionBase
        addEvent newEventBase3
        map stabilizeEventUid <$> viewNotifiations subscriberBase True
    it "listing and not checking with subscriptions and events should be filled twice" $ \arg ->
      assert ([eventBase2, eventBase3], [eventBase2, eventBase3]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        addEvent newEventBase3
        l1 <- viewNotifiations subscriberBase False
        l2 <- viewNotifiations subscriberBase False
        return (map stabilizeEventUid l1, map stabilizeEventUid l2)
    it "listing and checking with subscriptions and events should be filled once" $ \arg ->
      assert ([eventBase2, eventBase3], []) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        addEvent newEventBase3
        l1 <- viewNotifiations subscriberBase True
        l2 <- viewNotifiations subscriberBase True
        return (map stabilizeEventUid l1, map stabilizeEventUid l2)
    it "listing with subscriptions and events should be filled then empty after viewing" $ \arg ->
      assert ([eventBase2], [], [eventBase3]) $ runPersistance (configuration arg) $ do
        initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        subscribe subscriptionBase
        addEvent newEventBase2
        l1 <- viewNotifiations subscriberBase True
        l2 <- viewNotifiations subscriberBase True
        addEvent newEventBase3
        l3 <- viewNotifiations subscriberBase True
        return (map stabilizeEventUid l1, map stabilizeEventUid l2, map stabilizeEventUid l3)

spec :: Spec
spec = do
  let directMemory = persistSQLite $ pack ":memory:"
  describe "SQLite" $
    describe "law" $
      law $ const directMemory
  aroundWithRabbitMQ $ do
    let configuration = flip wrapPersistenceRabbitMQ directMemory
    describe "SQLite and RabbitMQ" $ do
      describe "law" $
        law $ configuration . fst
      describe "Realtime Notification" $
        it "listing with subscriptions and events should be filled then empty after viewing and live subscribing" $ \(channel, listenerChannel) ->
          assert ([eventBase2], Just [eventBase3], [eventBase3]) $ runPersistance (configuration channel) $ do
            initPersistence
            createKind kindBase
            createSubject subjectBase subjectBaseEventData
            addEvent newEventBase1
            subscribe subscriptionBase
            addEvent newEventBase2
            l1 <- viewNotifiations subscriberBase True
            mvar <- liftIO $ listen listenerChannel subscriberBase
            addEvent newEventBase3
            d1 <- liftIO $ readMVar mvar
            l2 <- viewNotifiations subscriberBase True
            return (map stabilizeEventUid l1, map stabilizeEventUid <$> d1, map stabilizeEventUid l2)

-- Fixtures
kindBaseUid :: KindUid
kindBaseUid = KindUid "cat"

actionBase1 :: Action
actionBase1 = Action "meow"

actionBase2 :: Action
actionBase2 = Action "pet"

kindBase :: Kind
kindBase = Kind kindBaseUid [actionBase1, actionBase2]

actionAdditional :: Action
actionAdditional = Action "groom"

kindEnriched :: Kind
kindEnriched = Kind kindBaseUid [Action "meow", Action "pet", actionAdditional]

subjectBaseUid :: SubjectUid
subjectBaseUid = SubjectUid "felix"

subjectBase :: Subject
subjectBase = Subject kindBaseUid subjectBaseUid

subjectBaseEventData :: EventData
subjectBaseEventData = EventData $ Bool True

eventBaseUUIDNeutral :: EventUid
eventBaseUUIDNeutral = EventUid $ fromJust $ fromString "00000000-0000-0000-0000-000000000000"

eventBaseInit :: Event
eventBaseInit = Event subjectBase eventBaseUUIDNeutral (Action "init") (EventData $ Bool True)

eventBase1 :: Event
eventBase1 = Event subjectBase eventBaseUUIDNeutral actionBase1 (EventData $ Bool True)

eventBase2 :: Event
eventBase2 = Event subjectBase eventBaseUUIDNeutral actionBase2 (EventData $ Bool False)

eventBase3 :: Event
eventBase3 = Event subjectBase eventBaseUUIDNeutral actionBase2 (EventData $ Bool True)

newEventBase1 :: NewEvent
newEventBase1 = NewEvent subjectBase actionBase1 (EventData $ Bool True)

newEventBase2 :: NewEvent
newEventBase2 = NewEvent subjectBase actionBase2 (EventData $ Bool False)

newEventBase3 :: NewEvent
newEventBase3 = NewEvent subjectBase actionBase2 (EventData $ Bool True)

newEventAdditionalInformation :: NewEvent
newEventAdditionalInformation = NewEvent subjectBase actionAdditional (EventData $ Bool False)

subscriberBase :: Subscriber
subscriberBase = Subscriber "marvin"

subscriptionBase :: Subscription
subscriptionBase = Subscription subscriberBase subjectBase

-- Helpers
assert :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
assert = flip shouldReturn

stabilizeEventUid :: Event -> Event
stabilizeEventUid x = x { eventUid = eventBaseUUIDNeutral }

stabilizeEventUidResult :: Either a EventUid -> Either a EventUid
stabilizeEventUidResult = fmap $ const eventBaseUUIDNeutral

listen :: Channel -> Subscriber -> IO (MVar (Maybe [Notification]))
listen channel subscriber = do
  mvar <- newEmptyMVar
  forkIO $ do
    result <- S.head $ S.intervalsOf 5 SF.toList $ pushedNotifiations channel subscriber
    putMVar mvar result
    return ()
  threadDelay $ 100 * 1000
  return mvar

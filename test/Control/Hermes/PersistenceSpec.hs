module Control.Hermes.PersistenceSpec (main, spec) where

import Control.Hermes.RabbitMQ
import Control.Hermes.RabbitMQUtils
import Control.Hermes.Types
import Control.Hermes.Persistence
import Control.Hermes.Persistence.SQLite
import Data.Aeson(Value(Bool))
import Data.Function(on)
import Data.Maybe(fromJust)
import Data.Text(pack)
import Data.UUID(fromString)
import Test.Hspec

main :: IO ()
main = hspec spec

law :: Persist p => (a -> Configuration p) -> SpecWith a
law configuration = do
  describe "Kind" $ do
    it "fetching a not created should be nothing" $ \arg ->
      assert Nothing $ runPersistance (configuration arg) $ do
        initPersistence
        fetchKind (kindUid kindBase)
    it "creating and fetching should be the created one" $ \arg ->
      assert (KindCreated, Just kindBase) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, fetchKindResult)
    it "creating a kind twice and fetching should return an error and fetch the first one" $ \arg ->
      assert (KindCreated, KindAlreadyExisting, Just kindBase) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        repeatedNewKindResult <- newKind kindBase
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, repeatedNewKindResult, fetchKindResult)
    it "creating then adding an action then fetching it should have all the actions" $ \arg ->
      assert (KindCreated, ActionAdded, Just kindEnriched) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult <- addAction kindBaseUid actionAdditional
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult, fetchKindResult)
    it "creating then adding an action twice then fetching it should have all the actions once" $ \arg ->
      assert (KindCreated, ActionAdded, ActionAlreadyExisting, Just kindEnriched) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult1 <- addAction kindBaseUid actionAdditional
        addActionResult2 <- addAction kindBaseUid actionAdditional
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult1, addActionResult2, fetchKindResult)
    it "creating then adding an existing action then fetching it should have all the actions once" $ \arg ->
      assert (KindCreated, ActionAlreadyExisting, Just kindBase) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult <- addAction kindBaseUid actionBase1
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult, fetchKindResult)
    it "creating with a duplicated action then fetching it should have all the actions once" $ \arg ->
      assert (KindCreated, Just kindBase) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind $ Kind kindBaseUid [actionBase1, actionBase2, actionBase1]
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, fetchKindResult)
  describe "Subject" $ do
    it "creating with an existing kind should be ok" $ \arg ->
      assert (KindCreated, SubjectCreated) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        newSubjectResult <- newSubject subjectBase
        return (newKindResult, newSubjectResult)
    it "creating a subject twice should return an error" $ \arg ->
      assert (KindCreated, SubjectCreated, SubjectAlreadyExisting) $ runPersistance (configuration arg) $ do
        initPersistence
        newKindResult <- newKind kindBase
        newSubjectResult1 <- newSubject subjectBase
        newSubjectResult2 <- newSubject subjectBase
        return (newKindResult, newSubjectResult1, newSubjectResult2)
  describe "Event" $ do
    it "creating with an existing subject should listed" $ \arg ->
      assert [eventBase1] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        events <- listEvents subjectBase
        return $ map stabilizeEventUid events
    it "creating two events should listed" $ \arg ->
      assert [eventBase1, eventBase2] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        newEvent newEventBase2
        events <- listEvents subjectBase
        return $ map stabilizeEventUid events
    it "creating an event event twice should listed" $ \arg ->
      assert (True, [eventBase1, eventBase1]) $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        newEvent newEventBase1
        events <- listEvents subjectBase
        let eventsUidDistinction e = and $ zipWith ((/=) `on` eventUid) e (tail e)
        return (eventsUidDistinction events, map stabilizeEventUid events)
  describe "Subscription" $ do
    it "creating with an existing subject should be ok" $ \arg ->
      assert (Just True) $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        creationUid <- newEvent newEventBase1
        subscriptionUid <- subscribe subscriptionBase
        return $ (== creationUid) <$> subscriptionUid
    it "creating with an unknown subject should be empty" $ \arg ->
      assert Nothing $ runPersistance (configuration arg) $ do
        initPersistence
        subscribe subscriptionBase
    it "deleting an existing subscription should be ok" $ \arg ->
      assert (Just True) $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        creationUid <- newEvent newEventBase1
        subscribe subscriptionBase
        unsubscriptionUid <- unsubscribe subscriptionBase
        return $ (== creationUid) <$> unsubscriptionUid
    it "deleting an unknown subscription should be empty" $ \arg ->
      assert Nothing $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        unsubscribe subscriptionBase
  describe "Notification" $ do
    it "listing an unknown subscriber should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        listNotifiations subscriberBase
    it "listing with a new subscription should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        listNotifiations subscriberBase
    it "listing with a deleted subscription and an event should be empty" $ \arg ->
      assert [] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        newEvent newEventBase2
        unsubscribe subscriptionBase
        newEvent newEventBase3
        listNotifiations subscriberBase
    it "listing with subscriptions and events should be filled" $ \arg ->
      assert [eventBase2, eventBase3] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        newEvent newEventBase2
        newEvent newEventBase3
        map stabilizeEventUid <$> listNotifiations subscriberBase
    it "listing with a subscription deleted and recreated and events should be filled with the last events" $ \arg ->
      assert [eventBase3] $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        newEvent newEventBase2
        unsubscribe subscriptionBase
        subscribe subscriptionBase
        newEvent newEventBase3
        map stabilizeEventUid <$> listNotifiations subscriberBase
    it "listing with subscriptions and events should be filled twice" $ \arg ->
      assert ([eventBase2, eventBase3], [eventBase2, eventBase3]) $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        newEvent newEventBase2
        newEvent newEventBase3
        l1 <- listNotifiations subscriberBase
        l2 <- listNotifiations subscriberBase
        return (map stabilizeEventUid l1, map stabilizeEventUid l2)
    it "listing with subscriptions and events should be filled then empty after viewing" $ \arg ->
      assert ([eventBase2], [], [eventBase3]) $ runPersistance (configuration arg) $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEvent newEventBase1
        subscribe subscriptionBase
        newEvent newEventBase2
        l1 <- listNotifiations subscriberBase
        viewNotifiations subscriberBase
        l2 <- listNotifiations subscriberBase
        newEvent newEventBase3
        l3 <- listNotifiations subscriberBase
        return (map stabilizeEventUid l1, map stabilizeEventUid l2, map stabilizeEventUid l3)

spec :: Spec
spec = do
  let directMemory = persistSQLite $ pack ":memory:"
  describe "SQLite" $
    describe "law" $
      law $ const directMemory
  aroundWithRabbitMQ $ describe "SQLite and RabbitMQ" $
    describe "law" $
      law $ flip wrapPersistenceRabbitMQ directMemory . fst

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

eventBaseUUIDNeutral :: EventUid
eventBaseUUIDNeutral = EventUid $ fromJust $ fromString "00000000-0000-0000-0000-000000000000"

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

subscriberBase :: Subscriber
subscriberBase = Subscriber "marvin"

subscriptionBase :: Subscription
subscriptionBase = Subscription subscriberBase subjectBase

-- Helpers
assert :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
assert = flip shouldReturn

stabilizeEventUid :: Event -> Event
stabilizeEventUid x = x { eventUid = eventBaseUUIDNeutral }

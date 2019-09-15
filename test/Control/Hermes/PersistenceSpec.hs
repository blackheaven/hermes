module Control.Hermes.PersistenceSpec (main, spec) where

import Control.Hermes.Types
import Control.Hermes.Persistence
import Control.Hermes.Persistence.SQLite
import Data.Aeson(Value(Bool))
import Data.Function(on)
import Data.Maybe(fromJust)
import Data.Text(pack)
import Data.UUID(fromString)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

law :: Persist p => Configuration p -> Spec
law x = do
  describe "Kind" $ do
    it "fetching a not created should be nothing" $ do
      assert Nothing $ runPersistance x $ do
        initPersistence
        fetchKind (kindUid kindBase)
    it "creating and fetching should be the created one" $ do
      assert (KindCreated, Just kindBase) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, fetchKindResult)
    it "creating a kind twice and fetching should return an error and fetch the first one" $ do
      assert (KindCreated, KindAlreadyExisting, Just kindBase) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        repeatedNewKindResult <- newKind kindBase
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, repeatedNewKindResult, fetchKindResult)
    it "creating then adding an action then fetching it should have all the actions" $ do
      assert (KindCreated, ActionAdded, Just kindEnriched) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult <- addAction kindBaseUid actionAdditional
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult, fetchKindResult)
    it "creating then adding an action twice then fetching it should have all the actions once" $ do
      assert (KindCreated, ActionAdded, ActionAlreadyExisting, Just kindEnriched) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult1 <- addAction kindBaseUid actionAdditional
        addActionResult2 <- addAction kindBaseUid actionAdditional
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult1, addActionResult2, fetchKindResult)
    it "creating then adding an existing action then fetching it should have all the actions once" $ do
      assert (KindCreated, ActionAlreadyExisting, Just kindBase) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        addActionResult <- addAction kindBaseUid actionBase1
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, addActionResult, fetchKindResult)
    it "creating with a duplicated action then fetching it should have all the actions once" $ do
      assert (KindCreated, Just kindBase) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind $ Kind kindBaseUid [actionBase1, actionBase2, actionBase1]
        fetchKindResult <- fetchKind kindBaseUid
        return (newKindResult, fetchKindResult)
  describe "Subject" $ do
    it "creating with a non existing kind should return an error" $ do
      assert SubjectKindDoesNotExists $ runPersistance x $ do
        initPersistence
        newSubject subjectBase
    it "creating with an existing kind should be ok" $ do
      assert (KindCreated, SubjectCreated) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        newSubjectResult <- newSubject subjectBase
        return (newKindResult, newSubjectResult)
    it "creating a subject twice should return an error" $ do
      assert (KindCreated, SubjectCreated, SubjectAlreadyExisting) $ runPersistance x $ do
        initPersistence
        newKindResult <- newKind kindBase
        newSubjectResult1 <- newSubject subjectBase
        newSubjectResult2 <- newSubject subjectBase
        return (newKindResult, newSubjectResult1, newSubjectResult2)
  describe "Event" $ do
    it "creating with a non existing subject should return an error" $ do
      assert (EventSubjectDoesNotExists, Nothing) $ runPersistance x $ do
        initPersistence
        newEventResult <- newEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        return (newEventResult, events)
    it "creating with an existing subject should listed" $ do
      assert (EventCreated, Just [eventBase1]) $ runPersistance x $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEventResult <- newEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        return (newEventResult, map stabilizeEventUid <$> events)
    it "creating two events should listed" $ do
      assert (EventCreated, EventCreated, Just [eventBase1, eventBase2]) $ runPersistance x $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEventResult1 <- newEvent newEventBase1
        newEventResult2 <- newEvent newEventBase2
        events <- listEvents kindBaseUid subjectBaseUid
        return (newEventResult1, newEventResult2, map stabilizeEventUid <$> events)
    it "creating an vent event twice should listed" $ do
      assert (EventCreated, EventCreated, Just True, Just [eventBase1, eventBase1]) $ runPersistance x $ do
        initPersistence
        newKind kindBase
        newSubject subjectBase
        newEventResult1 <- newEvent newEventBase1
        newEventResult2 <- newEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        let eventsUidDistinction e = all id $ zipWith ((/=) `on` eventUid) e (tail e)
        return (newEventResult1, newEventResult2, eventsUidDistinction <$> events, map stabilizeEventUid <$> events)

spec :: Spec
spec = do
  describe "SQLite" $ do
    describe "law" $ do
      law $ persistSQLite $ pack ":memory:"

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
eventBase1 = Event kindBaseUid subjectBaseUid eventBaseUUIDNeutral actionBase1 (EventData $ Bool True)

eventBase2 :: Event
eventBase2 = Event kindBaseUid subjectBaseUid eventBaseUUIDNeutral actionBase2 (EventData $ Bool False)

newEventBase1 :: NewEvent
newEventBase1 = NewEvent kindBaseUid subjectBaseUid actionBase1 (EventData $ Bool True)

newEventBase2 :: NewEvent
newEventBase2 = NewEvent kindBaseUid subjectBaseUid actionBase2 (EventData $ Bool False)

-- Helpers
assert :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
assert = flip shouldReturn

stabilizeEventUid :: Event -> Event
stabilizeEventUid x = x { eventUid = eventBaseUUIDNeutral }

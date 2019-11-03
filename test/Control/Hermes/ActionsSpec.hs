module Control.Hermes.ActionsSpec (main, spec) where

import Control.Hermes.Actions
import Control.Hermes.Types
import qualified Control.Hermes.Persistence as P
import qualified Control.Hermes.Persistence.SQLite as P
import Data.Aeson(Value(Bool))
import Data.Function(on)
import Data.Maybe(fromJust)
import Data.Text(pack)
import Data.UUID(fromString)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

law :: P.Persist p => P.Configuration p -> Spec
law x = do
  describe "createKind" $ do
    it "creating should work" $ do
      assert (Right ()) $ P.runPersistance x $ do
        P.initPersistence
        createKind kindBase
    it "creating a kind twice should return an error" $ do
      assert (Right (), Left KindAlreadyExistingError) $ P.runPersistance x $ do
        P.initPersistence
        createKindResult1 <- createKind kindBase
        createKindResult2 <- createKind kindBase
        return (createKindResult1, createKindResult2)
  describe "addAction" $ do
    it "creating then adding an action should work" $ do
      assert (Right (), Right ()) $ P.runPersistance x $ do
        P.initPersistence
        createKindResult <- createKind kindBase
        addActionResult <- addAction kindBaseUid actionAdditional
        return (createKindResult, addActionResult)
    it "creating then adding an action twice should fail the second time" $ do
      assert (Right (), Right (), Left ActionAlreadyExistingError) $ P.runPersistance x $ do
        P.initPersistence
        createKindResult <- createKind kindBase
        addActionResult1 <- addAction kindBaseUid actionAdditional
        addActionResult2 <- addAction kindBaseUid actionAdditional
        return (createKindResult, addActionResult1, addActionResult2)
    it "adding an action on an unknown kind should fail" $ do
      assert (Left ActionKindDoesNotExistsError) $ P.runPersistance x $ do
        P.initPersistence
        addAction kindBaseUid actionAdditional
  describe "createSubject" $ do
    it "creating with a non existing kind should return an error" $ do
      assert (Left SubjectKindDoesNotExistsError) $ P.runPersistance x $ do
        P.initPersistence
        createSubject subjectBase subjectBaseEventData
    it "creating with an existing kind should be ok" $ do
      assert (Right (), Right eventBaseUUIDNeutral) $ P.runPersistance x $ do
        P.initPersistence
        createKindResult <- createKind kindBase
        createSubjectResult <- createSubject subjectBase subjectBaseEventData
        return (createKindResult, stabilizeEventUidResult createSubjectResult)
    it "creating a subject twice should return an error" $ do
      assert (Right (), Right eventBaseUUIDNeutral, Left SubjectAlreadyExistingError) $ P.runPersistance x $ do
        P.initPersistence
        createKindResult <- createKind kindBase
        createSubjectResult1 <- createSubject subjectBase subjectBaseEventData
        createSubjectResult2 <- createSubject subjectBase subjectBaseEventData
        return (createKindResult, stabilizeEventUidResult createSubjectResult1, stabilizeEventUidResult createSubjectResult2)
  describe "Event" $ do
    it "creating with a non existing subject should return an error" $ do
      assert (Left EventSubjectDoesNotExistsError, Nothing) $ P.runPersistance x $ do
        P.initPersistence
        newEventResult <- addEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        return (newEventResult, events)
    it "creating with an unknown action should return an error" $ do
      assert (Left EventActionNotAllowedError, Just [eventBaseInit]) $ P.runPersistance x $ do
        P.initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEventResult <- addEvent newEventAdditionalInformation
        events <- listEvents kindBaseUid subjectBaseUid
        return (addEventResult, map stabilizeEventUid <$> events)
    it "creating with an existing subject should listed" $ do
      assert (Just [eventBaseInit, eventBase1]) $ P.runPersistance x $ do
        P.initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        return $ map stabilizeEventUid <$> events
    it "creating two events should listed" $ do
      assert (Just [eventBaseInit, eventBase1, eventBase2]) $ P.runPersistance x $ do
        P.initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        newEventResult1 <- addEvent newEventBase1
        newEventResult2 <- addEvent newEventBase2
        events <- listEvents kindBaseUid subjectBaseUid
        return $ map stabilizeEventUid <$> events
    it "creating an vent event twice should listed" $ do
      assert (Just True, Just [eventBaseInit, eventBase1, eventBase1]) $ P.runPersistance x $ do
        P.initPersistence
        createKind kindBase
        createSubject subjectBase subjectBaseEventData
        addEvent newEventBase1
        addEvent newEventBase1
        events <- listEvents kindBaseUid subjectBaseUid
        let eventsUidDistinction e = all id $ zipWith ((/=) `on` eventUid) e (tail e)
        return (eventsUidDistinction <$> events, map stabilizeEventUid <$> events)

spec :: Spec
spec = do
  describe "SQLite" $ do
    describe "law" $ do
      law $ P.persistSQLite $ pack ":memory:"

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
eventBaseInit = Event kindBaseUid subjectBaseUid eventBaseUUIDNeutral (Action "init") (EventData $ Bool True)

eventBase1 :: Event
eventBase1 = Event kindBaseUid subjectBaseUid eventBaseUUIDNeutral actionBase1 (EventData $ Bool True)

eventBase2 :: Event
eventBase2 = Event kindBaseUid subjectBaseUid eventBaseUUIDNeutral actionBase2 (EventData $ Bool False)

newEventBase1 :: P.NewEvent
newEventBase1 = P.NewEvent kindBaseUid subjectBaseUid actionBase1 (EventData $ Bool True)

newEventBase2 :: P.NewEvent
newEventBase2 = P.NewEvent kindBaseUid subjectBaseUid actionBase2 (EventData $ Bool False)

newEventAdditionalInformation :: P.NewEvent
newEventAdditionalInformation = P.NewEvent kindBaseUid subjectBaseUid actionAdditional (EventData $ Bool False)

-- Helpers
assert :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
assert = flip shouldReturn

stabilizeEventUid :: Event -> Event
stabilizeEventUid x = x { eventUid = eventBaseUUIDNeutral }

stabilizeEventUidResult :: Either a EventUid -> Either a EventUid
stabilizeEventUidResult = fmap $ const eventBaseUUIDNeutral

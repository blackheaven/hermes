module Control.Hermes.Actions(
                               createKind
                             , addAction
                             , createSubject
                             , addEvent
                             , listEvents
                             , subscribe
                             , unsubscribe
                             , viewNotifiations
                             , NewKindStatusError(..)
                             , NewActionStatusError(..)
                             , NewSubjectStatusError(..)
                             , NewEventStatusError(..)
                             ) where

import qualified Control.Hermes.Persistence as P
import Control.Hermes.Types
import Data.Maybe(isJust)

createKind :: P.Persist p => Kind -> p (Either NewKindStatusError ())
createKind kind = do
  result <- P.newKind kind
  return $ case result of
             P.KindCreated         -> Right ()
             P.KindAlreadyExisting -> Left KindAlreadyExistingError

addAction :: P.Persist p => KindUid -> Action -> p (Either NewActionStatusError ())
addAction kindUid action = do
  result <- P.addAction kindUid action
  return $ case result of
             P.ActionAdded -> Right ()
             P.ActionAlreadyExisting -> Left ActionAlreadyExistingError
             P.ActionKindNotExists -> Left ActionKindDoesNotExistsError

createSubject :: P.Persist p => Subject -> EventData -> p (Either NewSubjectStatusError EventUid)
createSubject subject eventData = do
  fetchedKind <- P.fetchKind $ subjectKind subject
  case fetchedKind of
    Nothing -> return $ Left SubjectKindDoesNotExistsError
    Just _  -> do
      subjectResult <- P.newSubject subject
      case subjectResult of
        P.SubjectAlreadyExisting -> return $ Left SubjectAlreadyExistingError
        P.SubjectCreated -> Right <$> P.newEvent (P.NewEvent subject (Action "init") eventData)

addEvent :: P.Persist p => P.NewEvent -> p (Either NewEventStatusError EventUid)
addEvent event = do
  allowedActions <- P.listAllowedActions (P.newEventSubject event)
  case allowedActions of
    Nothing      -> return $ Left EventSubjectDoesNotExistsError
    Just actions -> do
      if notElem (P.newEventAction event) actions
        then return $ Left EventActionNotAllowedError
        else Right <$> P.newEvent event

listEvents :: P.Persist p => Subject -> p (Maybe [Event])
listEvents subject = toMaybe <$> P.listEvents subject
  where toMaybe x = if null x then Nothing else Just x

subscribe :: P.Persist p => Subscription -> p Bool
subscribe subscription = isJust <$> P.subscribe subscription

unsubscribe :: P.Persist p => Subscription -> p Bool
unsubscribe subscription = isJust <$> P.unsubscribe subscription

viewNotifiations :: P.Persist p => Subscriber -> Bool -> p [Notification]
viewNotifiations subscriber view = do
  notifications <- P.listNotifiations subscriber
  if view then P.viewNotifiations subscriber else return ()
  return notifications

data NewKindStatusError =
                          KindAlreadyExistingError
                        deriving (Eq)

instance Show NewKindStatusError where
  show _ = "Kind already exists"

data NewActionStatusError =
                            ActionAlreadyExistingError
                          | ActionKindDoesNotExistsError
                          deriving (Eq)

instance Show NewActionStatusError where
  show x = case x of
             ActionAlreadyExistingError   -> "Action already exists"
             ActionKindDoesNotExistsError -> "Kind does not exists"

data NewSubjectStatusError =
                             SubjectAlreadyExistingError
                           | SubjectKindDoesNotExistsError
                           deriving (Eq)

instance Show NewSubjectStatusError where
  show x = case x of
             SubjectAlreadyExistingError   -> "Subject already exists"
             SubjectKindDoesNotExistsError -> "Kind does not exists"

data NewEventStatusError =
                           EventSubjectDoesNotExistsError
                         | EventActionNotAllowedError
                         deriving (Eq)

instance Show NewEventStatusError where
  show x = case x of
              EventSubjectDoesNotExistsError -> "Subject does not exist"
              EventActionNotAllowedError     -> "Action not allowed"

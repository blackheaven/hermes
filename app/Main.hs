{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import ApiType
import qualified Control.Hermes.Actions as A
import qualified Control.Hermes.Persistence as P
import qualified Control.Hermes.Persistence.SQLite as P
import qualified Control.Hermes.RabbitMQ as R
import qualified Control.Hermes.Types as T
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal(packChars)
import Data.Maybe(fromMaybe)
import Data.Text(pack)
import Data.UUID(toString)
import Control.Monad(join)
import Control.Monad.IO.Class(liftIO)
import Network.AMQP
import Network.HTTP.Base(urlDecode)
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Streamly
import qualified Streamly as S
import qualified Streamly.Prelude as S
import System.Environment(getArgs)
import GHC.Generics

newtype LiftedHandler p a = LiftHandler { unliftHandler :: p (Handler a) }

kind :: P.Persist p => KindUid -> KindBody -> LiftedHandler p ()
kind (KindUid uid) (KindBody actions) = LiftHandler $ do
    creation <- A.createKind $ T.Kind (T.KindUid uid) (map convertAction actions)
    case creation of
      Right () -> return $ return ()
      Left e   -> return $ throwError $ err400 { errBody = packChars $ show e }

subject :: P.Persist p => KindUid -> SubjectUid -> EventData -> LiftedHandler p NewEvent
subject kind subject body = LiftHandler $ do
    creation <- A.createSubject (T.Subject (kindUidFromApi kind) (subjectUidFromApi subject)) (eventDataFromApi body)
    case creation of
      Right uid -> return $ return $ eventUidToNewEventApi uid
      Left e    -> return $ throwError $ err400 { errBody = packChars $ show e }

event :: P.Persist p => ServerT EventEndpoint (LiftedHandler p)
event = post :<|> get
  where post :: P.Persist p => KindUid -> SubjectUid -> Action -> EventData -> LiftedHandler p NewEvent
        post kind subject action body = LiftHandler $ do
          creation <- A.addEvent (P.NewEvent (subjectFromApi kind subject) (actionFromApi action) (eventDataFromApi body))
          case creation of
            Right uid -> return $ return $ eventUidToNewEventApi uid
            Left e    -> return $ throwError $ err400 { errBody = packChars $ show e }
        get :: P.Persist p => KindUid -> SubjectUid -> LiftedHandler p [Event]
        get kind subject = LiftHandler $ do
          fetched <- A.listEvents $ subjectFromApi kind subject
          case fetched of
            Just xs -> return $ return $ eventToApi <$> xs
            Nothing -> return $ throwError $ err404 { errBody = packChars "Subject unknown" }

type PollRoute p = Subscriber -> LiftedHandler p (S.SerialT IO Notification)
subscribers :: P.Persist p => PollRoute p -> ServerT SubscriberEndpoint (LiftedHandler p)
subscribers poll = post :<|> delete :<|> get :<|> poll
  where post :: P.Persist p => Subscriber -> Subscription -> LiftedHandler p ()
        post subscriber subscription = LiftHandler $ do
          existingSubject <- A.subscribe (subscriptionFromApi subscriber subscription)
          if existingSubject
            then return $ return ()
            else return $ throwError $ err404 { errBody = packChars "Subject unknown" }
        delete :: P.Persist p => Subscriber -> Subscription -> LiftedHandler p ()
        delete subscriber subscription = LiftHandler $ do
          existingSubject <- A.unsubscribe (subscriptionFromApi subscriber subscription)
          if existingSubject
            then return $ return ()
            else return $ throwError $ err404 { errBody = packChars "Subject unknown" }
        get :: P.Persist p => Subscriber -> Bool -> LiftedHandler p [Notification]
        get subscriber view = LiftHandler $
          return . map notificationToApi <$> A.viewNotifiations (subscriberFromApi subscriber) view

pollSupported :: P.Persist p => Connection -> PollRoute p
pollSupported connection subscriber = LiftHandler $ return $ do
  channel <- liftIO $ openChannel connection
  return $ notificationToApi <$> R.pushedNotifiations channel (subscriberFromApi subscriber)

pollNotSupported :: P.Persist p => PollRoute p
pollNotSupported subscriber = LiftHandler $ return $ throwError $ err501 { errBody = packChars "polling is not configured" }

server :: P.Persist p => PollRoute p -> ServerT HermesAPI (LiftedHandler p)
server poll = kind :<|> subject :<|> event :<|> subscribers poll

app :: P.Persist p => PollRoute p ->  P.Configuration p -> Application
app poll p = serve hermesAPI $ hoistServer hermesAPI toHandler $ server poll
  where toHandler = join . liftIO . toIO
        toIO = P.runPersistance p . (P.initPersistence >>) . unliftHandler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [db] -> do
      putStrLn $ "Launching Hermes on " ++ db
      withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings $ app pollNotSupported $ P.persistSQLite $ pack db
    [db, rabbitMQURL] -> do
      putStrLn $ "Launching Hermes on " ++ db ++ " and " ++ rabbitMQURL
      withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        let rabbitMQParams = fromMaybe (error $ "invalid RabbitMQ url: " ++ rabbitMQURL) $ parseURI rabbitMQURL
        let rabbitMQAuth = fromMaybe (error "missing authority") $ uriAuthority rabbitMQParams
        let rqHostname = uriRegName rabbitMQAuth
        let rqPort = read $ tail $ uriPort rabbitMQAuth
        let rqVhost = pack $ urlDecode $ tail $ uriPath rabbitMQParams
        let rqLogin = pack $ takeWhile (/= ':') $ uriUserInfo rabbitMQAuth
        let rqPassword = pack $ init $ tail $ dropWhile (/= ':') $ uriUserInfo rabbitMQAuth
        connection <- liftIO $ openConnection' rqHostname rqPort rqVhost rqLogin rqPassword
        persistChannel <- openChannel connection
        runSettings settings $ app (pollSupported connection) $ R.wrapPersistenceRabbitMQ persistChannel $ P.persistSQLite $ pack db
    _    -> fail "usage: hermes db-path [rabbitmq-url]"

-- Helpers
kindUidFromApi :: KindUid -> T.KindUid
kindUidFromApi (KindUid x) = T.KindUid x

subjectUidFromApi :: SubjectUid -> T.SubjectUid
subjectUidFromApi (SubjectUid x) = T.SubjectUid x

subjectFromApi :: KindUid -> SubjectUid -> T.Subject
subjectFromApi kind subject = T.Subject (kindUidFromApi kind) (subjectUidFromApi subject)

actionFromApi :: Action -> T.Action
actionFromApi (Action x) = T.Action x

eventDataFromApi :: EventData -> T.EventData
eventDataFromApi (EventData x) = T.EventData x

subscriberFromApi :: Subscriber -> T.Subscriber
subscriberFromApi (Subscriber x) = T.Subscriber x

subscriptionFromApi :: Subscriber -> Subscription -> T.Subscription
subscriptionFromApi subscriber (Subscription kind subject) = T.Subscription (subscriberFromApi subscriber) (subjectFromApi kind subject)

kindUidToApi :: T.KindUid -> KindUid
kindUidToApi (T.KindUid x) = KindUid x

subjectUidToApi :: T.SubjectUid -> SubjectUid
subjectUidToApi (T.SubjectUid x) = SubjectUid x

actionToApi :: T.Action -> Action
actionToApi (T.Action x) = Action x

eventDataToApi :: T.EventData -> EventData
eventDataToApi (T.EventData x) = EventData x

eventUidToApi :: T.EventUid -> EventUid
eventUidToApi (T.EventUid x) = EventUid $ toString x

eventUidToNewEventApi :: T.EventUid -> NewEvent
eventUidToNewEventApi = NewEvent . eventUidToApi

eventToApi :: T.Event -> Event
eventToApi (T.Event _ u a d) = Event (eventUidToApi u) (actionToApi a) (eventDataToApi d)

notificationToApi :: T.Notification -> Notification
notificationToApi (T.Event (T.Subject k s) u a d) = Notification (kindUidToApi k) (subjectUidToApi s) (eventUidToApi u) (actionToApi a) (eventDataToApi d)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Control.Hermes.Types as T
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal(packChars)
import Data.Text(pack)
import Data.UUID(toString)
import Control.Monad(join)
import Control.Monad.IO.Class(liftIO)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment(getArgs)

newtype LiftedHandler p a = LiftHandler { unliftHandler :: (p (Handler a)) }

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
          creation <- A.addEvent (P.NewEvent (kindUidFromApi kind) (subjectUidFromApi subject) (actionFromApi action) (eventDataFromApi body))
          case creation of
            Right uid -> return $ return $ eventUidToNewEventApi uid
            Left e    -> return $ throwError $ err400 { errBody = packChars $ show e }
        get :: P.Persist p => KindUid -> SubjectUid -> LiftedHandler p [Event]
        get kind subject = LiftHandler $ do
          fetched <- A.listEvents (kindUidFromApi kind) (subjectUidFromApi subject)
          case fetched of
            Just xs -> return $ return $ eventToApi <$> xs
            Nothing -> return $ throwError $ err404 { errBody = packChars $ "Subject unknown" }

server :: P.Persist p => ServerT HermesAPI (LiftedHandler p)
server = kind :<|> subject :<|> event

app :: P.Persist p => P.Configuration p -> Application
app p = serve hermesAPI $ hoistServer hermesAPI toHandler server
  where toHandler = join . liftIO . toIO
        toIO = (P.runPersistance p . (P.initPersistence >>) . unliftHandler)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [db] -> do
      putStrLn $ "Launching Hermes on " ++ db
      withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings $ app $ P.persistSQLite $ pack db
    _    -> fail "Hermes waits for one argument: the db path"

-- Helpers
kindUidFromApi :: KindUid -> T.KindUid
kindUidFromApi (KindUid x) = T.KindUid x

subjectUidFromApi :: SubjectUid -> T.SubjectUid
subjectUidFromApi (SubjectUid x) = T.SubjectUid x

actionFromApi :: Action -> T.Action
actionFromApi (Action x) = T.Action x

eventDataFromApi :: EventData -> T.EventData
eventDataFromApi (EventData x) = T.EventData x

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
eventToApi (T.Event _ _ u (T.Action a) (T.EventData d)) = Event (eventUidToApi u) (Action a) (EventData d)

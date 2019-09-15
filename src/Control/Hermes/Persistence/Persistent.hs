{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Hermes.Persistence.Persistent where

import Control.Hermes.Persistence as H
import Control.Hermes.Types as H
import Control.Monad((<=<))
import Database.Persist.Class
import Database.Persist.Sql
import Data.Aeson
import Data.Aeson.Types
import Data.Text(Text, pack, unpack)
import Data.UUID(UUID, fromString, toString)
import Data.Either.Combinators(maybeToRight)

instance PersistField H.KindUid where
    toPersistValue (H.KindUid k) = PersistText $ pack k
    fromPersistValue = fmap (H.KindUid . unpack) . fromPersistValueText

instance PersistFieldSql H.KindUid where
    sqlType _ = SqlString

instance PersistField H.SubjectUid where
    toPersistValue (H.SubjectUid s) = PersistText $ pack s
    fromPersistValue = fmap (H.SubjectUid . unpack) . fromPersistValueText

instance PersistFieldSql H.SubjectUid where
    sqlType _ = SqlString

instance PersistField H.EventUid where
    toPersistValue (H.EventUid e) = PersistText . pack $ toString e
    fromPersistValue = fmap H.EventUid . (>>= parseUUID) . toString . fromPersistValueText
      where parseUUID :: String -> Either Text UUID
            parseUUID x = maybeToRight (pack $ "'" ++ x ++ "' is not parsable as UUID") $ fromString x
            toString :: Either Text Text -> Either Text String
            toString = fmap unpack

instance PersistFieldSql H.EventUid where
    sqlType _ = SqlString

instance ToJSON H.KindUid
instance FromJSON H.KindUid

instance ToJSON H.Action
instance FromJSON H.Action

instance ToJSON H.Kind
instance FromJSON H.Kind

instance ToJSON H.SubjectUid
instance FromJSON H.SubjectUid

instance ToJSON H.Subject
instance FromJSON H.Subject

instance ToJSON H.EventUid
instance FromJSON H.EventUid

instance ToJSON H.EventData
instance FromJSON H.EventData

instance ToJSON H.Event
instance FromJSON H.Event

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
import Data.Aeson
import Data.Aeson.Types
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)

kind :: KindUid -> KindBody -> Handler ()
kind uid body = return ()

subject :: KindUid -> SubjectUid -> EventData -> Handler NewEvent
subject kind subject body = return $ NewEvent $ EventUid "se.01"

event = post :<|> get
  where post :: KindUid -> SubjectUid -> Action -> EventData -> Handler NewEvent
        post kind subject action body = return $ NewEvent $ EventUid "ee.01"
        get :: KindUid -> SubjectUid -> Handler [Event]
        get kind subject = return []

server :: Server HermesAPI
server = kind :<|> subject :<|> event

app :: Application
app = serve hermesAPI server

main :: IO ()
-- main = run 8080 app
main =
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app

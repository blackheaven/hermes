{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Control.Hermes.RabbitMQUtils (
                                      aroundWithRabbitMQ
                                    , withRabbitMQ
                                    ) where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( MonadIO, liftIO )
import qualified Data.Text                     as T
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Concurrent.MVar
import Data.Maybe(fromJust, isJust)
import Network.AMQP
import           System.Exit(ExitCode(..))
import           System.Process                 ( readProcessWithExitCode )
import           TestContainers.Hspec
import Test.Hspec
import Test.Hspec.Core.Hooks

aroundWithRabbitMQ :: SpecWith (Channel, Channel) -> Spec
aroundWithRabbitMQ = aroundAll withRabbitMQ

-- https://github.com/hspec/hspec/issues/255#issuecomment-585664769
-- | Wrap an action around the given spec.
aroundAll :: (ActionWith a -> IO ()) -> SpecWith a -> Spec
aroundAll actionWithAToUnit = aroundAllWith (const . actionWithAToUnit)

-- | Wrap an action around the given spec. Changes the arg type inside.
aroundAllWith :: (ActionWith a -> ActionWith b) -> SpecWith a -> SpecWith b
aroundAllWith actionWithAToB specWithA = do
  done <- runIO newEmptyMVar
  beforeAllWith
    (\b -> do
       -- The mutable reference is needed because @actionWithAToB@
       -- returns a (), not an @a@, as we'd like.
       mvarA <- newEmptyMVar
       -- The async thread is needed because @actionWithAToB@
       -- allocates and then /frees/ the resource, so we should block
       -- it from doing so until @afterAll@ has executed. Hence the
       -- @done@ var.
       blocker <-
         async
           (actionWithAToB
              (\a -> do
                 -- We make the @a@ value immediately available to the
                 -- @takeMVar@ below, to be passed to @specWithA@ down
                 -- the road.
                 putMVar mvarA a
                 -- Wait for the test to be done before allowing
                 -- @actionWithAToB@ to clean up resources.
                 takeMVar done)
              b)
       -- If @actionWithAToB@ fails then it should rethrow the
       -- exception to this thread before we try to @takeMVar@ on a
       -- var that will never be filled. This gives better error
       -- messages than a "blocked indefinitely".
       --
       -- Link is non-blocking.
       link blocker
       -- This should block until the @a@ is allocated.
       takeMVar mvarA)
    (afterAll (const (putMVar done ())) specWithA)

data MemoizedValue a = EmptyValue
                     | MemoizedValue a
                     | FailedValue E.SomeException


-- | Run a custom action before the first spec item, modifying the contents
beforeAllWith :: (b -> IO a) -> SpecWith a -> SpecWith b
beforeAllWith action spec = do
  mvar <- runIO (newMVar EmptyValue)

  flip aroundWith spec $ \actionExpectingA bValue -> do
    aValue <- memoize mvar action bValue
    actionExpectingA aValue

memoize :: MVar (MemoizedValue a) -> (b -> IO a) -> (b -> IO a)
memoize mvar action bValue = do
  result <- modifyMVar mvar $ \mv -> case mv of
    EmptyValue ->
      E.try (action bValue) >>= \case
        Left err -> return (FailedValue err, Left err)
        Right aValue -> return (MemoizedValue aValue, Right aValue)
    MemoizedValue aValue -> return (mv, Right aValue)
    FailedValue err -> return (mv, Left err)

  case result of
    Left err -> E.throwIO err
    Right aValue -> return aValue

withRabbitMQ :: ((Channel, Channel) -> IO ()) -> IO ()
withRabbitMQ = withContainers containers

getIp :: Container -> IO (Maybe String)
getIp (Container id _ _) = checkAndFormat <$> readProcessWithExitCode
  "docker"
  [ "inspect"
  , "-f"
  , "'{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
  , T.unpack id
  ]
  ""
  where format = init . init . tail
        checkAndFormat (ExitSuccess, out, _) = Just $ format out
        checkAndFormat _ = Nothing

isReady :: Container -> (String -> Bool) -> IO Bool
isReady (Container id _ _) check = analyze <$> readProcessWithExitCode
  "docker"
  [ "logs"
  , T.unpack id
  ]
  ""
  where analyze (ExitSuccess, out, _) = check out
        analyze _ = False

containers :: MonadDocker m => m (Channel, Channel)
containers = do
  let (login, password) = ("guest", "guest")
  container <- run (fromTag "rabbitmq:3.8.4") defaultContainerRequest
  ip <- liftIO $ fmap fromJust $ untilM isJust $ getIp container
  liftIO $ untilM Prelude.id $ isReady container $ any (startsWith " completed with ") . lines
  connection <- liftIO $ openConnection ip "/" login password
  liftIO $ (,) <$> openChannel connection <*> openChannel connection

untilM :: (MonadIO m, Monad m) => (a -> Bool) -> m a -> m a
untilM f m = go
    where
        go = do
            x <- m
            if f x
              then return x
              else liftIO (threadDelay (1000 * 1000)) >> go

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith xs ys = xs == take (length xs) ys

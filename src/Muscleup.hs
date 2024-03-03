{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Muscleup (runApp) where

import App (AppConfig (..), AppEnv (..), AppM (..), Environment (..))
import Control.Monad.Catch as Catch (
  Handler (..),
  catches,
 )
import Control.Monad.Trans.Resource (runResourceT)
import Data.UUID (nil)
import Data.UUID.V4 (nextRandom)
import Exercise
import Network.Wai.Handler.Warp (run)
import Servant (Handler (..), ServerError (..), err400, err500)
import Servant.Server (Application)
import Servant.Server.Generic (genericServeT)
import Server.RootServer (api)

runApp :: IO ()
runApp = runResourceT $ do
  testSession <- liftIO mkTestSession
  sessionRef <- newIORef testSession
  let env = AppEnv (AppConfig Dev) sessionRef
  liftIO $ runServer env

-- this is mostly to get domain errors up and running.
-- and to test converting them to ServerError and
-- catching IO Exceptions
data AppError
  = SomethingHappened Text
  | SomethingBadHappened Text
  | ServantError Servant.ServerError
  deriving (Show, Exception)

appErrorToServerError :: AppError -> ServerError
appErrorToServerError (SomethingHappened msg) =
  err500{errBody = encodeUtf8 msg}
appErrorToServerError (SomethingBadHappened msg) =
  err400{errBody = encodeUtf8 msg}
appErrorToServerError (ServantError err) = err

{- | Catches all exceptions thrown by the input function and maps them to
 servant errors, resulting in an ExceptT over IO which should be free from IO
 exceptions.
-}
convertException :: IO a -> ExceptT ServerError IO a
convertException fa =
  ExceptT $
    catches
      (Right <$> fa)
      [ Catch.Handler $ \(e :: AppError) ->
          return $ Left $ appErrorToServerError e
      , Catch.Handler $ \(e :: SomeException) ->
          return $ Left err500{errBody = show e}
      ]

handleException ::
  IO a ->
  ExceptT ServerError IO a
handleException =
  convertException

appToHandler :: AppEnv -> AppM a -> Servant.Handler a
appToHandler env (AppM m) = do
  Servant.Handler $
    handleException (runReaderT m env)

app :: (forall a. AppM a -> Servant.Handler a) -> Application
app f = genericServeT f api

mkTestSession :: IO Session
mkTestSession = do
  squatId <- ExerciseId <$> nextRandom
  benchId <- ExerciseId <$> nextRandom
  rowId <- ExerciseId <$> nextRandom
  set1Id <- ExerciseSetId <$> nextRandom
  set2Id <- ExerciseSetId <$> nextRandom
  set3Id <- ExerciseSetId <$> nextRandom
  let squat = Exercise squatId (ExerciseName "squat")
      bench = Exercise benchId (ExerciseName "bench")
      row = Exercise rowId (ExerciseName "row")
      testSet1 =
        ExerciseSet
          { id = set1Id
          , exercise = squat.name
          , reps = 10
          , resistance = Kg 10
          , completed = False
          }
      testSet2 =
        ExerciseSet
          { id = set2Id
          , exercise = bench.name
          , reps = 10
          , resistance = Kg 10
          , completed = False
          }
      testSet3 =
        ExerciseSet
          { id = set3Id
          , exercise = row.name
          , reps = 10
          , resistance = Kg 10
          , completed = False
          }
  pure $
    Session
      { id = SessionId nil
      , sets =
          [ testSet1
          , testSet2
          , testSet3
          ]
      }

runServer :: AppEnv -> IO ()
runServer env = do
  putStrLn "running"
  run 8080 $
    app $
      appToHandler
        env

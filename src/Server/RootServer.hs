module Server.RootServer where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Generics.Product (HasType (..))
import Data.Maybe (fromJust)
import Exercise (ExerciseSet (..), ExerciseSetId, Resistance (..), Session (..))
import Html.Session (container, renderSession, renderSet)
import Lucid (Html)
import Route.Root (RootRoutes (..))
import Servant.Server.Generic (AsServerT)

handleUpdateSet ::
  MonadIO m =>
  MonadReader r m =>
  HasType (IORef Session) r =>
  ExerciseSetId ->
  Int ->
  Int ->
  m (Html ())
handleUpdateSet setId newResistanceAmount newSets = do
  sessionRef <- asks $ getTyped @(IORef Session)
  session <- readIORef sessionRef
  let oldSet = fromJust $ find (\s -> s.id == setId) session.sets
      newSet =
        oldSet
          { reps = newSets
          , resistance = updateResistance newResistanceAmount oldSet.resistance
          , completed = True
          }
  print newSet
  pure $ renderSet newSet
 where
  updateResistance :: Int -> Resistance -> Resistance
  updateResistance amount = \case
    Kg _ -> Kg amount
    Pounds _ -> Pounds amount

handleSession ::
  MonadIO m =>
  MonadReader r m =>
  HasType (IORef Session) r =>
  m (Html ())
handleSession = do
  sessionRef <- asks getTyped
  session <- readIORef sessionRef
  pure $ container "session" $ renderSession session

api ::
  MonadUnliftIO m =>
  MonadThrow m =>
  MonadCatch m =>
  MonadReader r m =>
  HasType (IORef Session) r =>
  RootRoutes (AsServerT m)
api =
  RootRoutes
    { ping = pure "pong"
    , session = handleSession
    , updateSet = handleUpdateSet
    }

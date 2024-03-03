module Exercise where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Servant (FromHttpApiData)

--------------------------------------------------------------------------------

newtype ExerciseId = ExerciseId {unExerciseId :: UUID}
  deriving stock (Show, Generic)

newtype ExerciseName = ExerciseName {unExerciseName :: Text}
  deriving stock (Show, Generic)

data Exercise = Exercise
  { id :: ExerciseId
  , name :: ExerciseName
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------

newtype WorkoutId = WorkoutId {unWorkoutId :: UUID}
  deriving (Show, Generic)

data Workout = Workout
  { id :: WorkoutId
  , time :: Text
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------

newtype SessionId = SessionId {unSessionId :: UUID}
  deriving stock (Show, Generic)

data Session = Session
  { id :: SessionId
  , sets :: [ExerciseSet]
  --, started :: UTCTime
  --, finished :: UTCTime
  }

--------------------------------------------------------------------------------

data Resistance
  = Kg Int
  | Pounds Int
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------

newtype ExerciseSetId = ExerciseSetId {unExerciseSetId :: UUID}
  deriving stock (Show, Generic, Eq)
  deriving newtype (FromHttpApiData)

data ExerciseSet = ExerciseSet
  { id :: ExerciseSetId
  , exercise :: ExerciseName
  , reps :: Int
  , resistance :: Resistance
  --, completed :: Maybe UTCTime
  , completed :: Bool
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------

data ExercisePlan

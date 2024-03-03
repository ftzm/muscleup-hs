{-# LANGUAGE DataKinds #-}

module Route.Root (RootRoutes (..)) where

import Exercise (ExerciseSetId)
import Lucid (Html)
import Servant (Get, PlainText, QueryParam', Required, Strict, (:-), (:>))
import ServantLucid (HTML)

data RootRoutes mode = RootRoutes
  { -- routes :: mode :- NamedRoutes Routes
    ping :: mode :- "ping" :> Get '[PlainText] String
  , session :: mode :- "session" :> Get '[HTML] (Html ())
  , updateSet ::
      mode
        :- "updateSet"
          :> QueryParam' '[Required, Strict] "setId" ExerciseSetId
          :> QueryParam' '[Required, Strict] "resistanceAmount" Int
          :> QueryParam' '[Required, Strict] "reps" Int
          :> Get '[HTML] (Html ())
  }
  deriving (Generic)

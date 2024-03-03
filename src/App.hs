module App where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Exercise (Session)

data Environment
  = Prod
  | Dev
  | Test

data AppEnv = AppEnv
  { appConfig :: AppConfig
  , session :: IORef Session
  -- , logEnv :: LogEnv
  -- , logContexts :: LogContexts
  -- , namespace :: Namespace
  }
  deriving (Generic)

newtype AppConfig = AppConfig
  { environment :: Environment
  }

newtype AppM a = AppM (ReaderT AppEnv IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadThrow
    , MonadCatch
    , MonadUnliftIO
    )

module FDSC.App
  ( MonadApp,
    MonadLog (..),
    LogT,
    AppM,
    runAppM,
  )
where

import Network.HTTP.Req (HttpException, MonadHttp (..), Req, defaultHttpConfig, runReq)
import Relude

type MonadApp m = (MonadHttp m, MonadLog m)

class MonadLog m where
  log :: String -> m ()

newtype LogT m a = LogT (ReaderT Bool m a)
  deriving (Functor, Applicative, Monad, MonadReader Bool, MonadIO, MonadTrans)

instance (MonadIO m) => MonadLog (LogT m) where
  log :: String -> LogT m ()
  log msg = do
    isDebug <- ask
    if isDebug then liftIO (putStrLn msg) else pure ()

instance (MonadHttp m) => MonadHttp (LogT m) where
  handleHttpException :: HttpException -> LogT m a
  handleHttpException = lift . handleHttpException

type AppM t = LogT Req t

runAppM :: Bool -> AppM t -> IO t
runAppM debug (LogT r) = runReq defaultHttpConfig $ runReaderT r debug

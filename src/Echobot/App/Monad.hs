{-# LANGUAGE InstanceSigs #-}

module Echobot.App.Monad
  ( App(..)
  , AppEnv
  , runApp
  )
where

import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Echobot.App.Env                ( Env )
import           Network.HTTP.Req               ( MonadHttp(..)
                                                , HttpException(..)
                                                )
import           UnliftIO.Exception             ( throwIO )

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving newtype ( Functor, Applicative, Monad
                     , MonadReader AppEnv, MonadIO, MonadUnliftIO)

instance MonadHttp App where
  handleHttpException :: HttpException -> App a
  handleHttpException = throwIO
  {-# INLINE handleHttpException #-}

runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp

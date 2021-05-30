{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Echobot.App.Monad
  ( App(..)
  , AppEnv
  , runApp
  )
where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Echobot.App.Env                ( Env )
import           Network.HTTP.Req               ( MonadHttp(..)
                                                , HttpException(..)
                                                )

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving newtype ( Functor, Applicative, Monad
                     , MonadReader AppEnv, MonadIO, MonadUnliftIO)

instance MonadHttp App where
  handleHttpException :: Network.HTTP.Req.HttpException -> App a
  handleHttpException = liftIO . throwIO

runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp

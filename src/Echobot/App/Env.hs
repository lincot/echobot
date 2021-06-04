{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Echobot.App.Env
  ( Env(..)
  , Has(..)
  , grab
  , initialisedField
  )
where

import           Colog                          ( HasLog(..)
                                                , LogAction(..)
                                                , Message
                                                )
import           Echobot.Core.Dflts             ( Dflts )
import           Echobot.Core.Msgs              ( Msgs )
import           Echobot.Core.Irc               ( Irc )
import           Echobot.Core.Matrix            ( Matrix )
import           Echobot.Core.Mattermost        ( Mattermost )
import           Echobot.Core.Telegram          ( Telegram )
import           Echobot.Core.Xmpp              ( Xmpp )
import           UnliftIO.Exception             ( try )
import           Control.Exception              ( RecConError(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )

data Env (m :: Type -> Type) = Env
  { envLogAction  :: !(LogAction m Message)
  , envDflts      :: !Dflts
  , envMsgs       :: !Msgs
  , envIrc        :: Irc
  , envMatrix     :: Matrix
  , envMattermost :: Mattermost
  , envTelegram   :: Telegram
  , envXmpp       :: Xmpp
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newAction env = env { envLogAction = newAction }
  {-# INLINE setLogAction #-}

class Has field env where
  obtain :: env -> field

instance Has (LogAction m Message) (Env m) where obtain = envLogAction
instance Has Dflts                 (Env m) where obtain = envDflts
instance Has Msgs                  (Env m) where obtain = envMsgs
instance Has Irc                   (Env m) where obtain = envIrc
instance Has Matrix                (Env m) where obtain = envMatrix
instance Has Mattermost            (Env m) where obtain = envMattermost
instance Has Telegram              (Env m) where obtain = envTelegram
instance Has Xmpp                  (Env m) where obtain = envXmpp

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

initialisedField
  :: forall field env m
   . (MonadReader env m, Has field env, MonadUnliftIO m)
  => m Bool
initialisedField = do
  f   <- grab @field
  eef <- try $ evaluateWHNF f
  pure $ case eef of
    Left RecConError {} -> False
    _                   -> True

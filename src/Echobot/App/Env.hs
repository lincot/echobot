{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Echobot.App.Env
  ( Env(..)
  , Has(..)
  , grab
  )
where

import           Echobot.Types.Dflts            ( Dflts )
import           Echobot.Types.Msgs             ( Msgs )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Irc              ( Irc )
import           Echobot.Types.Matrix           ( Matrix )
import           Echobot.Types.Mattermost       ( Mattermost )
import           Echobot.Types.Telegram         ( Telegram )
import           Echobot.Types.Xmpp             ( Xmpp )

data Env (m :: Type -> Type) = Env
  { envDflts      :: !Dflts
  , envMsgs       :: !Msgs
  , envSeverity   :: !Severity
  , envIrc        :: Irc
  , envMatrix     :: Matrix
  , envMattermost :: Mattermost
  , envTelegram   :: Telegram
  , envXmpp       :: Xmpp
  }

class Has field env where
  obtain :: env -> field

instance Has Dflts      (Env m) where obtain = envDflts
instance Has Msgs       (Env m) where obtain = envMsgs
instance Has Severity   (Env m) where obtain = envSeverity
instance Has Irc        (Env m) where obtain = envIrc
instance Has Matrix     (Env m) where obtain = envMatrix
instance Has Mattermost (Env m) where obtain = envMattermost
instance Has Telegram   (Env m) where obtain = envTelegram
instance Has Xmpp       (Env m) where obtain = envXmpp

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

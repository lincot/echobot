module Echobot.Types.ToConnect
  ( ToConnect(..)
  ) where

data ToConnect = ToConnect
  { connectIrc, connectMatrix, connectMattermost, connectTelegram, connectXmpp
      :: !Bool
  }

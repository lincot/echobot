module Echobot.Core.ToConnect
  ( ToConnect(..)
  )
where

data ToConnect = ToConnect
  { connectIrc        :: !Bool
  , connectMatrix     :: !Bool
  , connectMattermost :: !Bool
  , connectTelegram   :: !Bool
  , connectXmpp       :: !Bool
  }

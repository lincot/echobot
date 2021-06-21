module Echobot.Types.Irc
  ( Irc(..)
  , IrcC(..)
  )
where

data Irc = Irc
  { ircSocket :: !Handle
  , ircChan   :: !Text
  }

data IrcC = IrcC
  { cIrcHost, cIrcPort :: !String
  , cIrcChan, cIrcNick, cIrcName :: !Text
  }

module Echobot.Types.Irc
  ( Irc(..)
  , IrcC(..)
  ) where

data Irc = Irc
  { ircSocket :: !Handle
  , ircChan   :: !Text
  }

data IrcC = IrcC
  { ircHost, ircPort          :: !String
  , ircChan, ircNick, ircName :: !Text
  }

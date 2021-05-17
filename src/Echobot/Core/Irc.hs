module Echobot.Core.Irc
  ( Irc(..)
  , IrcC(..)
  )
where

data Irc = Irc
  { ircSocket :: !Handle
  , ircChan   :: !Text
  , ircNick   :: !Text
  , ircName   :: !Text
  }

data IrcC = IrcC
  { cIrcHost :: !String
  , cIrcPort :: !String
  , cIrcChan :: !Text
  , cIrcNick :: !Text
  , cIrcName :: !Text
  }

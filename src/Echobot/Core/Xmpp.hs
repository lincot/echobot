module Echobot.Core.Xmpp
  ( Xmpp(..)
  , XmppC(..)
  )
where

import           Network.Xmpp                   ( Session )

newtype Xmpp = Xmpp
  { xmppSession :: Session
  }

data XmppC = XmppC
  { cXmppHost :: !String
  , cXmppNick :: !Text
  , cXmppPswd :: !Text
  }

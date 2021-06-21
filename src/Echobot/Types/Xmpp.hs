module Echobot.Types.Xmpp
  ( Xmpp
  , XmppC(..)
  )
where

import           Network.Xmpp                   ( Session )

type Xmpp = Session

data XmppC = XmppC
  { cXmppHost :: !String
  , cXmppNick, cXmppPswd :: !Text
  }
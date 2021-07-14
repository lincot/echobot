module Echobot.Types.Xmpp
  ( Xmpp
  , XmppC(..)
  ) where

import           Network.Xmpp                   ( Session )

type Xmpp = Session

data XmppC = XmppC
  { xmppHost           :: !String
  , xmppNick, xmppPswd :: !Text
  }

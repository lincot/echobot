module Echobot.Core.Mattermost
  ( Mattermost
  , MattermostC(..)
  )
where

import           Network.Mattermost.Types       ( Session )

type Mattermost = Session

data MattermostC = MattermostC
  { cMmHost :: !Text
  , cMmPort :: !Int
  , cMmPath :: !Text
  , cMmNick :: !Text
  , cMmPswd :: !Text
  }

module Echobot.Types.Mattermost
  ( Mattermost
  , MattermostC(..)
  ) where

import           Network.Mattermost.Types       ( Session )

type Mattermost = Session

data MattermostC = MattermostC
  { mmHost                 :: !Text
  , mmPort                 :: !Int
  , mmPath, mmNick, mmPswd :: !Text
  }

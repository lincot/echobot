module Echobot.Types.User
  ( BotMode(..)
  , User(..)
  , UsersR
  , newUser
  ) where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Dflts            ( Dflts(..) )

data BotMode = NormalMode | AwaitingRepeatCountMode

-- bot mode separately for each user
-- so others can't set your value in a group chat
data User = User
  { userMode        :: !BotMode
  , userRepeatCount :: !Int
  }

type UsersR u = IORef (HashMap u User)

newUser :: BotMode -> App User
newUser userMode = do
  Dflts {..} <- grab
  pure User { .. }

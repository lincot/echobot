module Echobot.Core.Users
  ( BotMode(..)
  , User(..)
  , Users
  )
where

data BotMode = NormalMode | AwaitingRepeatCountMode

type Users u = IORef (HashMap u User)

-- bot mode separately for each user
-- so others can't set your value in a group chat
data User = User
  { userMode        :: !BotMode
  , userRepeatCount :: !Int
  }

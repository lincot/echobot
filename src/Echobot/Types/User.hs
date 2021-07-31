module Echobot.Types.User
  ( User(..)
  , UserMode(..)
  , UsersR
  , newUser
  ) where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Dflts            ( Dflts(..) )

data UserMode = Normal | PendingRepeatCount

data User = User
  { userMode        :: UserMode
  , userRepeatCount :: Int
  }

type UsersR u = IORef (HashMap u User)

newUser :: UserMode -> App User
newUser userMode = do
  Dflts{..} <- grab
  pure User{..}

module Echobot.Types.Bot
  ( Bot(..)
  ) where

import           Echobot.App.Monad              ( App )
import           Echobot.Types.User             ( UsersR )

data Bot c u = Bot
  { getMessages :: App [(c, u, Text)]
  , sendMessage :: c -> Text -> App ()
  , disableBot  :: App ()
  , botName     :: Text
  , users       :: UsersR u
  }

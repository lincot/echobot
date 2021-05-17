module Echobot.Core.Bot
  ( Bot(..)
  )
where

import           Echobot.App.Monad              ( App )
import           Echobot.Core.Users             ( Users )

data Bot c u = Bot
  { startBot    :: App ()
  , disableBot  :: App ()
  , getMessages :: App [(c, u, Text)]
  , sendMessage :: c -> Text -> App ()
  , botName     :: Text
  , users       :: Users u
  }

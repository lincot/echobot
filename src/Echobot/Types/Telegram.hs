module Echobot.Types.Telegram
  ( Telegram(..)
  , TelegramC(..)
  )
where

data Telegram = Telegram
  { tgToken  :: !Text
  , tgOffset :: !(IORef Int)
  }

data TelegramC = TelegramC
  { cTgToken  :: !Text
  , cTgOffset :: !Int
  }

module Echobot.Core.Telegram
  ( Telegram(..)
  , TelegramC(..)
  )
where

data Telegram = Telegram
  { tgOffset :: !(IORef Int)
  , tgToken  :: !Text
  }

data TelegramC = TelegramC
  { cTgOffset :: !Int
  , cTgToken  :: !Text
  }

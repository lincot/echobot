module Echobot.Types.Telegram
  ( Telegram(..)
  , TelegramC(..)
  ) where

data Telegram = Telegram
  { tgToken   :: !Text
  , tgOffsetR :: !(IORef Int)
  }

data TelegramC = TelegramC
  { tgToken  :: !Text
  , tgOffset :: !Int
  }

module Echobot.Types.Msgs
  ( Msgs(..)
  ) where

data Msgs = Msgs
  { helpMsg, repeat1Msg, repeat2Msg, invalidMsg :: !Text
  }

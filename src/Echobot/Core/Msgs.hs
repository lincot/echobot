module Echobot.Core.Msgs
  ( Msgs(..)
  )
where

data Msgs = Msgs
  { helpMsg    :: !Text
  , repeat1Msg :: !Text
  , repeat2Msg :: !Text
  , invalidMsg :: !Text
  }

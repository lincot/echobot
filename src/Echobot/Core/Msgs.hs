module Echobot.Core.Msgs
  ( Msgs(..)
  )
where

data Msgs = Msgs
  { helpMsg    :: !Text
  , repeatMsg  :: !Text
  , invalidMsg :: !Text
  }

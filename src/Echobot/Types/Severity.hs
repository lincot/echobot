module Echobot.Types.Severity
  ( Severity(..)
  )
where

-- Debug, Info, Warning, Error, Notify
data Severity = D | I | W | E | N deriving (Show, Read, Eq, Ord)

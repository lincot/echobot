module Echobot.Types.Severity
  ( Severity(..)
  )
where

-- Debug, Info, Warning, Error
data Severity = D | I | W | E deriving (Show, Read, Eq, Ord)

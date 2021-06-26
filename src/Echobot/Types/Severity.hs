module Echobot.Types.Severity
  ( Severity(..)
  )
where

-- Debug, Info, Warning, Error, Notify
data Severity = D | I | W | E | N deriving (Show, Read, Eq, Ord)

instance ToText Severity where
  toText = \case
    D -> "\ESC[92mD" -- green
    I -> "\ESC[94mI" -- blue
    W -> "\ESC[93mW" -- yellow
    E -> "\ESC[91mE" -- red
    N -> "\ESC[96mN" -- cyan

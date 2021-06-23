module Echobot.Log
  ( log
  , Severity(..)
  )
where

import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Severity         ( Severity(..) )

green, blue, yellow, red, magenta, reset :: Text
green    = "\ESC[92m"
blue     = "\ESC[94m"
yellow   = "\ESC[93m"
red      = "\ESC[91m"
magenta  = "\ESC[95m"
reset    = "\ESC[0m"

log :: Severity -> Text -> Text -> App ()
log sev loc msg = do
  minSev <- grab
  time   <- liftIO getCurrentTime
  let sevCol = case sev of
        D -> green
        I -> blue
        W -> yellow
        E -> red
  unless (sev < minSev) $ putText
    $  sevCol <> show sev <> " [" <> pad 35 (show time <> "] ")
    <> magenta <> "[" <> pad 10 (loc <> "] ")
    <> reset <> msg <> "\n"

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "

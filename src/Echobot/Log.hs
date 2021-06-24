module Echobot.Log
  ( log , logIO
  , Severity(..)
  )
where

import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Severity         ( Severity(..) )

green, blue, yellow, red, magenta, reset, cyan :: Text
red      = "\ESC[91m"
green    = "\ESC[92m"
yellow   = "\ESC[93m"
blue     = "\ESC[94m"
magenta  = "\ESC[95m"
cyan     = "\ESC[96m"
reset    = "\ESC[0m"

log :: Severity -> Text -> Text -> App ()
log sev loc msg = do
  minSev <- grab
  unless (sev < minSev) $ liftIO $ logIO sev loc msg

logIO :: Severity -> Text -> Text -> IO ()
logIO sev loc msg = do
  time <- getCurrentTime
  let sevCol = case sev of
        D -> green
        I -> blue
        W -> yellow
        E -> red
        N -> cyan
  putText
    $  sevCol <> show sev <> " [" <> pad 35 (show time <> "] ")
    <> magenta <> "[" <> pad 10 (loc <> "] ")
    <> reset <> msg <> "\n"

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "

module Echobot.Log
  ( log
  , logIO
  , Severity(..)
  )
where

import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Types.Severity         ( Severity(..) )


log :: Severity -> Text -> Text -> App ()
log sev loc msg = do
  minSev <- grab
  liftIO $ logIO minSev sev loc msg

logIO :: Severity -> Severity -> Text -> Text -> IO ()
logIO minSev sev loc msg = unless (sev < minSev) $ do
  time <- getCurrentTime
  putText
    $  toText sev <> " [" <> pad 35 (show time <> "] ")
    <> magenta    <>  "[" <> pad 10 (loc       <> "] ")
    <> reset <> msg <> "\n"
 where
  magenta = "\ESC[95m"
  reset   = "\ESC[0m"
  pad n t = t <> T.replicate (n - T.length t) " "

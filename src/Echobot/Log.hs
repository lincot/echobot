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
import           System.Console.ANSI

log :: Severity -> Text -> Text -> App ()
log sev loc msg = do
  minSev <- grab
  time   <- liftIO getCurrentTime
  let sevCol = case sev of
        D -> Green
        I -> Blue
        W -> Yellow
        E -> Red
      setSevCol = toText $ setSGRCode [SetColor Foreground Vivid sevCol]
      setMagCol = toText $ setSGRCode [SetColor Foreground Vivid Magenta]
      resetCol  = toText $ setSGRCode [Reset]
  unless (sev < minSev) $ putText
    $  setSevCol <> show sev <> " [" <> pad 35 (show time <> "] ")
    <> setMagCol <> "[" <> pad 10 (loc <> "] ")
    <> resetCol <> msg <> "\n"

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "

module Echobot.Bots.Irc
  ( ircBot
  )
where

import qualified Data.Text                     as T
import           Data.Text.IO                   ( hGetLine
                                                , hPutStrLn
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Irc              ( Irc(..) )
import           UnliftIO.IO                    ( hClose )

ircBot :: App (Bot Text Text)
ircBot = Bot getMessagesIrc sendMessageIrc disableIrc "IRC" <$> newIORef mempty

writeIrc :: Text -> Text -> App ()
writeIrc cmd args = do
  Irc {..} <- grab
  let line = cmd <> " " <> args
  log D "IRC" $ "writing\n" <> line
  liftIO $ hPutStrLn ircSocket line

getMessagesIrc :: App [(Text, Text, Text)]
getMessagesIrc = do
  Irc {..} <- grab
  line     <- liftIO $ hGetLine ircSocket
  log D "IRC" $ "reading\n" <> line
  let (src, cmd, _, msg) = parseLine line
  case cmd of
    "PING" -> do
      writeIrc "PONG" msg
      getMessagesIrc
    "PRIVMSG" -> if "/utility-bot" `T.isInfixOf` src
      then getMessagesIrc
      else pure [(ircChan, src, msg)]
    "366" -> log I "IRC" "joined channel" >> getMessagesIrc
    _     -> getMessagesIrc

sendMessageIrc :: Text -> Text -> App ()
sendMessageIrc chan msg =
  mapM_ (writeIrc "PRIVMSG" . ((chan <> " :") <>)) $ lines msg

disableIrc :: App ()
disableIrc = do
  Irc {..} <- grab
  log D "IRC" "closing handle"
  hClose ircSocket

parseLine :: Text -> (Text, Text, Text, Text)
parseLine (T.stripPrefix "PING " -> Just suf) = ("", "PING", "", T.init suf)
parseLine line = (src, cmd, target, msg)
 where
  (src   , xs) = T.break p line
  (cmd   , ys) = T.break p $ T.tail xs
  (target, ms) = T.break p $ T.tail ys
  msg          = T.tail . T.tail . T.init $ ms
  p            = (== ' ')

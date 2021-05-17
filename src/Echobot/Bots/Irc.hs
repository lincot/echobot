{-# LANGUAGE ViewPatterns #-}

module Echobot.Bots.Irc
  ( ircBot
  )
where

import           Colog                          ( pattern D
                                                , pattern I
                                                , log
                                                )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hGetLine
                                                , hPutStr
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Irc               ( Irc(..) )
import           UnliftIO.IO                    ( hClose )

ircBot :: App (Bot Text Text)
ircBot = Bot startIrc disableIrc getMessagesIrc sendMessageIrc "IRC"
  <$> newIORef mempty

writeIrc :: Text -> Text -> App ()
writeIrc cmd args = do
  irc <- grab
  let line = cmd <> " " <> args
  log D $ "[IRC] writing:\n" <> line
  liftIO $ hPutStr (ircSocket irc) (line <> "\n")

startIrc :: App ()
startIrc = do
  irc <- grab
  let nick = ircNick irc
  writeIrc "NICK" nick
  writeIrc "USER" (nick <> " 0 * :" <> ircName irc)
  writeIrc "JOIN" (ircChan irc)
  log I "[IRC] connected to channel"

disableIrc :: App ()
disableIrc = do
  irc <- grab
  hClose (ircSocket irc)

getMessagesIrc :: App [(Text, Text, Text)]
getMessagesIrc = do
  irc  <- grab
  line <- liftIO $ hGetLine (ircSocket irc)
  log D $ "[IRC] reading:\n" <> line
  let (src, cmd, _, msg) = parseLine line
  case cmd of
    "PING" -> do
      writeIrc "PONG" msg
      getMessagesIrc
    "PRIVMSG" -> if "/utility-bot" `T.isInfixOf` src
      then getMessagesIrc
      else return [(ircChan irc, src, msg)]
    _ -> getMessagesIrc

sendMessageIrc :: Text -> Text -> App ()
sendMessageIrc chan msg =
  mapM_ (writeIrc "PRIVMSG" . ((chan <> " :") <>)) (lines msg)

parseLine :: Text -> (Text, Text, Text, Text)
parseLine (T.stripPrefix "PING " -> Just suf) = ("", "PING", "", T.init suf)
parseLine line = (src, cmd, target, msg)
 where
  (src   , xs) = T.break p line
  (cmd   , ys) = T.break p (T.tail xs)
  (target, ms) = T.break p (T.tail ys)
  msg          = T.tail . T.tail . T.init $ ms
  p            = (== ' ')

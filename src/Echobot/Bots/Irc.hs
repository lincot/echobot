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
                                                , hPutStrLn
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Irc               ( Irc(..) )
import           UnliftIO.IO                    ( hClose )

ircBot :: App (Bot Text Text)
ircBot = Bot getMessagesIrc sendMessageIrc disableIrc "IRC" <$> newIORef mempty

writeIrc :: Text -> Text -> App ()
writeIrc cmd args = do
  irc <- grab
  let line = cmd <> " " <> args
  log D $ "[IRC] writing:\n" <> line
  liftIO $ hPutStrLn (ircSocket irc) line

getMessagesIrc :: App [(Text, Text, Text)]
getMessagesIrc = do
  irc  <- grab
  line <- liftIO $ hGetLine $ ircSocket irc
  log D $ "[IRC] reading:\n" <> line
  let (src, cmd, _, msg) = parseLine line
  case cmd of
    "PING" -> do
      writeIrc "PONG" msg
      getMessagesIrc
    "PRIVMSG" -> if "/utility-bot" `T.isInfixOf` src
      then getMessagesIrc
      else pure [(ircChan irc, src, msg)]
    _ -> getMessagesIrc

sendMessageIrc :: Text -> Text -> App ()
sendMessageIrc chan msg =
  mapM_ (writeIrc "PRIVMSG" . ((chan <> " :") <>)) $ lines msg

disableIrc :: App ()
disableIrc = do
  irc <- grab
  log I "[IRC] closing handle"
  hClose $ ircSocket irc

parseLine :: Text -> (Text, Text, Text, Text)
parseLine (T.stripPrefix "PING " -> Just suf) = ("", "PING", "", T.init suf)
parseLine line = (src, cmd, target, msg)
 where
  (src   , xs) = T.break p line
  (cmd   , ys) = T.break p $ T.tail xs
  (target, ms) = T.break p $ T.tail ys
  msg          = T.tail . T.tail . T.init $ ms
  p            = (== ' ')

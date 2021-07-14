module Echobot.Bots.Irc
  ( ircBot
  , ircConnect
  ) where

import           Control.Exception              ( bracketOnError )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hGetLine
                                                , hPutStrLn
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Irc              ( Irc(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Network.Socket
import           UnliftIO.IO                    ( hClose )

handleConnect :: String -> String -> IO Handle
handleConnect host port = do
  addr : _ <- getAddrInfo Nothing (Just host) (Just port)
  sock     <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  socketToHandle sock ReadWriteMode

chanConnect :: Text -> Text -> Text -> Handle -> IO ()
chanConnect chan nick name h = mapM_
  (hPutStrLn h)
  ["NICK " <> nick, "USER " <> nick <> " 0 * :" <> name, "JOIN " <> chan]

ircConnect :: String -> String -> Text -> Text -> Text -> IO Handle
ircConnect host port chan nick name = bracketOnError
  (handleConnect host port)
  hClose
  ((*>) <$> chanConnect chan nick name <*> pure)

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
sendMessageIrc chan = mapM_ (writeIrc "PRIVMSG" . ((chan <> " :") <>)) . lines

disableIrc :: App ()
disableIrc = do
  Irc {..} <- grab
  log D "IRC" "closing handle"
  hClose ircSocket

parseLine :: Text -> (Text, Text, Text, Text)
parseLine (T.stripPrefix "PING " -> Just suf) = ("", "PING", "", T.init suf)
parseLine line = (src, cmd, target, msg)
 where
  (src   , xs) = spl line
  (cmd   , ys) = spl xs
  (target, ms) = spl ys
  msg          = T.tail . T.tail . T.init $ ms
  spl          = T.break (== ' ') . T.tail

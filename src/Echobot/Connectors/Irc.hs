module Echobot.Connectors.Irc
  ( ircConnect
  )
where

import           Control.Exception              ( bracketOnError )
import           Data.Text.IO                   ( hPutStrLn )
import           Echobot.Connectors.Handle      ( handleConnect )
import           System.IO                      ( hClose )

ircConnect :: String -> String -> Text -> Text -> Text -> IO Handle
ircConnect host port chan nick name = bracketOnError
  (handleConnect host port)
  hClose
  ((*>) <$> chanConnect chan nick name <*> pure)

chanConnect :: Text -> Text -> Text -> Handle -> IO ()
chanConnect chan nick name h = mapM_
  (hPutStrLn h)
  ["NICK " <> nick, "USER " <> nick <> " 0 * :" <> name, "JOIN " <> chan]

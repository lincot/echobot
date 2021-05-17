module Echobot.Connectors.Handle
  ( handleConnect
  )
where

import           Network.Socket

handleConnect :: String -> String -> IO Handle
handleConnect host port = do
  addr : _ <- getAddrInfo Nothing (Just host) (Just port)
  sock     <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  socketToHandle sock ReadWriteMode

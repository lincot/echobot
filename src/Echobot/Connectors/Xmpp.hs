module Echobot.Connectors.Xmpp
  ( xmppConnect
  )
where

import           Data.Default                   ( def )
import           Network.Xmpp                   ( session
                                                , scramSha1
                                                , Session
                                                )

xmppConnect :: String -> Text -> Text -> IO Session
xmppConnect host nick pswd = do
  ees <- session host (Just (const [scramSha1 nick Nothing pswd], Nothing)) def
  case ees of
    Right s -> pure s
    Left  e -> error $ "[XMPP] " <> show e

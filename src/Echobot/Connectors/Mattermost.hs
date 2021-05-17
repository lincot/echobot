module Echobot.Connectors.Mattermost
  ( mattermostConnect
  )
where

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types
import           Network.Mattermost.Util

mattermostConnect :: Text -> Int -> Text -> Text -> Text -> IO Session
mattermostConnect host port path nick pswd = do
  cd <- initConnectionData host
                           port
                           path
                           (ConnectHTTPS True)
                           defaultConnectionPoolConfig
  fst <$> (hoistE =<< mmLogin cd (Login nick pswd))

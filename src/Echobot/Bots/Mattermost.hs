module Echobot.Bots.Mattermost
  ( mattermostBot
  , mattermostConnect
  ) where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types
import           Network.Mattermost.Util

deriving via Text instance ToText UserId

mattermostConnect :: Text -> Int -> Text -> Text -> Text -> IO Session
mattermostConnect host port path nick pswd = do
  cd <- initConnectionData host
                           port
                           path
                           (ConnectHTTPS True)
                           defaultConnectionPoolConfig
  fst <$> (hoistE =<< mmLogin cd (Login nick pswd))

mattermostBot :: App (Bot ChannelId UserId)
mattermostBot =
  Bot getMessagesMM sendMessageMM pass "Mattermost" <$> newIORef mempty

getMessagesMM :: App [(ChannelId, UserId, Text)]
getMessagesMM = do
  mm    <- grab
  teams <- liftIO $ mmGetUsersTeams UserMe mm
  (pure . concat =<<) $ forM teams $ \team -> do
    chans <- liftIO $ mmGetChannelsForUser UserMe (getId team) mm
    (pure . concat =<<) $ forM chans $ \chan -> do
      let chanId = getId chan
      posts <- liftIO
        $ mmGetPostsForChannel chanId defaultPostQuery mm
      (pure . catMaybes =<<)
        $ forM (reverse . toList $ postsOrder posts)
        $ \pId -> case postsPosts posts !? pId of
            Nothing -> do
              log D "Mattermost" "could not find a post by PostId"
              pure Nothing
            Just p -> case postUserId p of
              Nothing -> do
                log D "Mattermost" "got a post without UserId"
                pure Nothing
              Just uId ->
                pure $ Just (chanId, uId, unsafeUserText $ postMessage p)

sendMessageMM :: ChannelId -> Text -> App ()
sendMessageMM chanId msg = do
  mm <- grab
  let post = RawPost { rawPostChannelId = chanId
                     , rawPostMessage   = msg
                     , rawPostFileIds   = mempty
                     , rawPostRootId    = Nothing
                     }
  liftIO $ void $ mmCreatePost post mm

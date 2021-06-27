{-# OPTIONS -Wno-orphans #-}

module Echobot.Bots.Mattermost
  ( mattermostBot
  )
where

import qualified Data.HashMap.Strict           as HMS
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Bot              ( Bot(..) )
import           Network.Mattermost.Endpoints   ( defaultPostQuery
                                                , mmCreatePost
                                                , mmGetChannelsForUser
                                                , mmGetPostsForChannel
                                                , mmGetUsersTeams
                                                )
import           Network.Mattermost.Types       ( ChannelId
                                                , UserId
                                                , RawPost(..)
                                                , UserParam(UserMe)
                                                , getId
                                                , Post(..)
                                                , Posts(..)
                                                , unsafeUserText
                                                , Id(..)
                                                , UserId(..)
                                                )

deriving via Text instance ToText UserId

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
        $ \pId -> case HMS.lookup pId $ postsPosts posts of
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
  let post = RawPost
        { rawPostChannelId = chanId
        , rawPostMessage   = msg
        , rawPostFileIds   = mempty
        , rawPostRootId    = Nothing
        }
  liftIO $ void $ mmCreatePost post mm

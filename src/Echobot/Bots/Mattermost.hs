{-# OPTIONS -Wno-orphans #-}

module Echobot.Bots.Mattermost
  ( mattermostBot
  )
where

import           Data.HashMap.Strict            ( lookup )
import           Colog                          ( pattern D
                                                , log
                                                )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Core.Bot               ( Bot(..) )
import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types       ( Channel
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

mattermostBot :: App (Bot Channel UserId)
mattermostBot =
  Bot getMessagesMM sendMessageMM pass "Mattermost" <$> newIORef mempty

getMessagesMM :: App [(Channel, UserId, Text)]
getMessagesMM = do
  mm    <- grab
  teams <- liftIO $ mmGetUsersTeams UserMe mm
  (pure . concat =<<) $ forM teams $ \team -> do
    chans <- liftIO $ mmGetChannelsForUser UserMe (getId team) mm
    (pure . concat =<<) $ forM chans $ \chan -> do
      posts <- liftIO
        $ mmGetPostsForChannel (getId chan) defaultPostQuery mm
      (pure . catMaybes =<<)
        $ forM (reverse . toList $ postsOrder posts)
        $ \pId -> case lookup pId $ postsPosts posts of
            Nothing -> do
              log D "[Mattermost] could not find a post by PostId"
              pure Nothing
            Just p -> case postUserId p of
              Nothing -> do
                log D "[Mattermost] got a post without UserId"
                pure Nothing
              Just uId ->
                pure $ Just (chan, uId, unsafeUserText $ postMessage p)

sendMessageMM :: Channel -> Text -> App ()
sendMessageMM chan msg = do
  mm <- grab
  let post = RawPost
        { rawPostChannelId = getId chan
        , rawPostMessage   = msg
        , rawPostFileIds   = mempty
        , rawPostRootId    = Nothing
        }
  liftIO $ void $ mmCreatePost post mm

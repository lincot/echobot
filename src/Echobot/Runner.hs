module Echobot.Runner
  ( botRunner
  )
where

import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Db                     ( getUser
                                                , putUser
                                                )
import           Echobot.Log                    ( log )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Msgs             ( Msgs(..) )
import           Echobot.Types.Users            ( BotMode(..)
                                                , User(..)
                                                , newUser
                                                )
import           UnliftIO.Exception             ( onException )

botRunner :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
botRunner bot@Bot {..} = do
  log I botName "bot started"
  onException (runBot bot) disableBot

runBot :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> App ()
runBot bot@Bot {..} = forever $ mapM
  (\(chan, uid, msg) -> do
    log I botName $ "received message from " <> toText uid <> "\n" <> msg
    react bot chan uid msg
  ) =<< getMessages

react :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> Text -> App ()
react bot@Bot {..} chan uid msg = do
  muser <- getUser users uid
  case muser of
    Just user -> case userMode user of
      AwaitingRepeatCountMode -> reactRepeatCount bot chan uid user msg
      NormalMode              -> reactNormal      bot chan uid user msg
    Nothing -> do
      log D botName $ toText uid <> " is absent in db"
      reactNew bot chan uid msg

reactNew :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> Text -> App ()
reactNew bot chan _ msg | msg `elem` ["/help", "/start"] = do
  Msgs {..} <- grab
  sendMessage' bot chan helpMsg
reactNew bot@Bot {..} chan uid "/repeat" = do
  Msgs {..} <- grab
  user@User {..} <- newUser AwaitingRepeatCountMode
  sendMessage' bot chan $ repeat1Msg <> show userRepeatCount <> repeat2Msg
  putUser users uid user
reactNew bot chan _ msg = do
  User {..} <- newUser NormalMode
  replicateM_ userRepeatCount $ sendMessage' bot chan msg

reactNormal :: (Eq u, Hashable u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactNormal bot chan _ _ msg | msg `elem` ["/help", "/start"] = do
  Msgs {..} <- grab
  sendMessage' bot chan helpMsg
reactNormal bot@Bot {..} chan uid user@User {..} "/repeat" = do
  Msgs {..} <- grab
  sendMessage' bot chan $ repeat1Msg <> show userRepeatCount <> repeat2Msg
  putUser users uid user { userMode = AwaitingRepeatCountMode }
reactNormal bot chan _ User {..} msg =
  replicateM_ userRepeatCount $ sendMessage' bot chan msg

reactRepeatCount :: (Eq u, Hashable u, ToText u) =>
  Bot c u -> c -> u -> User -> Text -> App ()
reactRepeatCount bot@Bot {..} chan uid user msg =
  case readMaybe $ toString msg of
    Just c -> if
      | c < 0 -> invalid "a negative"
      | 5 < c -> invalid "too big"
      | otherwise -> do
        log I botName $ "changing repeat count to " <> show c
          <> " for " <> toText uid
        putUser users uid user { userRepeatCount = c, userMode = NormalMode }
    Nothing -> invalid "not a"
 where
  invalid n = do
    log W botName $ "got " <> n <> " number from " <> toText uid
    Msgs {..} <- grab
    sendMessage' bot chan invalidMsg

sendMessage' :: Bot c u -> c -> Text -> App ()
sendMessage' Bot{..} chan msg = do
  log I botName $ "sending message\n" <> msg
  sendMessage chan msg

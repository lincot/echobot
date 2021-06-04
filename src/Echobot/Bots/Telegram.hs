{-# OPTIONS -Wno-orphans #-}

module Echobot.Bots.Telegram
  ( telegramBot
  )
where

import           Colog                          ( pattern D
                                                , pattern W
                                                , log
                                                )
import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                )
import           Data.Aeson.Types               ( parseEither )
import           Echobot.App.Monad              ( App )
import           Echobot.App.Env                ( grab )
import           Echobot.Bots.Telegram.Types    ( TgResponse(TgResponse)
                                                , TgUpdate(TgUpdate, update_id)
                                                , TgMessage(TgMessage)
                                                , TgChat(TgChat)
                                                , TgUser(TgUser)
                                                )
import           Echobot.Core.Bot               ( Bot(..) )
import           Echobot.Core.Telegram          ( Telegram(..) )
import           Network.HTTP.Req

instance ToText Int where
  toText = show
  {-# INLINE toText #-}

telegramBot :: App (Bot Int Int)
telegramBot = Bot pass pass getMessagesTg sendMessageTg "Telegram"
  <$> newIORef mempty

reqTg :: (FromJSON a, Show a) =>
  Text -> Text -> Network.HTTP.Req.Option 'Https -> App (Either Text a)
reqTg token method params = do
  let url = https "api.telegram.org" /: "bot" <> token /: method
  rb <- responseBody <$> req GET url NoReqBody jsonResponse params
  case parseEither parseJSON rb of
    Right (TgResponse _ (Just result)) -> do
      log D $ "[Telegram] got:\n" <> show result
      pure $ Right result
    Right (TgResponse (Just desc) _) -> pure $ Left desc
    Right (TgResponse _ _) -> pure $ Left "got nothing :O"
    Left e -> pure $ Left $ toText e

getUpdates :: App [TgUpdate]
getUpdates = do
  tg <- grab
  offset <- readIORef $ tgOffset tg
  r <- reqTg (tgToken tg) "getUpdates"
    $  "offset"  =: offset
    <> "timeout" =: (10 :: Int)
  case r of
    Left e -> do
      log W $ "[Telegram] could not get updates\n" <> e
      getUpdates
    Right upds -> case viaNonEmpty last upds of
      Just upd -> do
        writeIORef (tgOffset tg) $ update_id upd + 1
        pure upds
      Nothing  -> getUpdates

getMessagesTg :: App [(Int, Int, Text)]
getMessagesTg = do
  upds <- getUpdates
  pure $ catMaybes $ parseUpdate <$> upds

parseUpdate :: TgUpdate -> Maybe (Int, Int, Text)
parseUpdate = \case
  TgUpdate _ (Just (TgMessage (Just (TgUser usr)) (TgChat chat) (Just msg))) ->
    Just (chat, usr, msg)
  _ -> Nothing

sendMessageTg :: Int -> Text -> App ()
sendMessageTg chat msg = do
  tg <- grab
  r <- reqTg (tgToken tg) "sendMessage"
    $  "chat_id" =: chat
    <> "text"    =: msg
  case r of
    Left e -> do
      log W $ "[Telegram] could not deliver message\n" <> e
      sendMessageTg chat msg
    Right TgMessage {} -> pass

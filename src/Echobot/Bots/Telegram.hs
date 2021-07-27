module Echobot.Bots.Telegram
  ( telegramBot
  ) where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                )
import           Data.Aeson.Types               ( parseEither )
import           Echobot.App.Env                ( grab )
import           Echobot.App.Monad              ( App )
import           Echobot.Bots.Telegram.Types    ( TgChat(TgChat)
                                                , TgMessage(TgMessage)
                                                , TgResponse(TgResponse)
                                                , TgUpdate(TgUpdate, update_id)
                                                , TgUser(TgUser)
                                                )
import           Echobot.Log                    ( log )
import           Echobot.Types.Bot              ( Bot(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Telegram         ( Telegram(..) )
import           Network.HTTP.Req

instance ToText Int where
  toText = show
  {-# INLINE toText #-}

telegramBot :: App (Bot Int Int)
telegramBot =
  Bot getMessagesTg sendMessageTg pass "Telegram" <$> newIORef mempty

reqTg :: (FromJSON a, Show a) =>
  Text -> Text -> Network.HTTP.Req.Option 'Https -> App (Either Text a)
reqTg token method params = do
  let url = https "api.telegram.org" /: "bot" <> token /: method
  rb <- responseBody <$> req GET url NoReqBody jsonResponse params
  case parseEither parseJSON rb of
    Right (TgResponse _ (Just result)) -> do
      log D "Telegram" $ "got\n" <> show result
      pure $ Right result
    Right (TgResponse (Just desc) _) -> pure $ Left desc
    Right (TgResponse _ _) -> pure $ Left "got nothing :O"
    Left e -> pure $ Left $ toText e

getUpdates :: App [TgUpdate]
getUpdates = do
  Telegram {..} <- grab
  offset        <- readIORef tgOffsetR
  r <- reqTg tgToken "getUpdates"
    $  "offset"  =: offset
    <> "timeout" =: (10 :: Int)
  case r of
    Left e -> do
      log E "Telegram" $ "could not get updates\n" <> e
      getUpdates
    Right upds -> case viaNonEmpty last upds of
      Just upd -> do
        writeIORef tgOffsetR $ update_id upd + 1
        pure upds
      Nothing  -> getUpdates

getMessagesTg :: App [(Int, Int, Text)]
getMessagesTg = do
  upds <- getUpdates
  pure
    [ (chat, usr, msg)
    | TgUpdate _ (Just (TgMessage (Just (TgUser usr)) (TgChat chat) (Just msg)))
    <- upds ]

sendMessageTg :: Int -> Text -> App ()
sendMessageTg chat msg = do
  Telegram {..} <- grab
  r <- reqTg tgToken "sendMessage"
    $  "chat_id" =: chat
    <> "text"    =: msg
  case r of
    Left e -> do
      log E "Telegram" $ "could not deliver message\n" <> e
      sendMessageTg chat msg
    Right TgMessage{} -> pass

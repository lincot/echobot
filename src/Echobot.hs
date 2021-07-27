{-# OPTIONS -Wno-missing-fields #-}

module Echobot
  ( main
  ) where

import           Data.List                      ( foldr1 )
import qualified Data.Text                     as T
import           Echobot.App.Env                ( Env(..) )
import           Echobot.App.Monad              ( App
                                                , AppEnv
                                                , runApp
                                                )
import           Echobot.Bots.Irc               ( ircBot
                                                , ircConnect
                                                )
import           Echobot.Bots.Matrix            ( matrixBot )
import           Echobot.Bots.Mattermost        ( mattermostBot
                                                , mattermostConnect
                                                )
import           Echobot.Bots.Telegram          ( telegramBot )
import           Echobot.Bots.Xmpp              ( xmppBot
                                                , xmppConnect
                                                )
import           Echobot.Config                 ( Config(..)
                                                , loadConfig
                                                )
import           Echobot.Log                    ( logIO )
import           Echobot.Run                    ( runBot )
import           Echobot.Types.Irc              ( Irc(..)
                                                , IrcC(..)
                                                )
import           Echobot.Types.Matrix           ( Matrix(..)
                                                , MatrixC(..)
                                                )
import           Echobot.Types.Mattermost       ( MattermostC(..) )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.Telegram         ( Telegram(..)
                                                , TelegramC(..)
                                                )
import           Echobot.Types.ToConnect        ( ToConnect(..) )
import           Echobot.Types.Xmpp             ( XmppC(..) )
import           UnliftIO.Async                 ( Concurrently(..)
                                                , runConcurrently
                                                )

mkEnvAndApps :: Config -> IO (AppEnv, [App ()])
mkEnvAndApps Config {..} =
  (, snd <$> l) <$> (foldr1 (>=>) ((`fst` envSeverity) <$> l) $ Env {..})
 where
  l = snd <$> filter (`fst` cConnect)
    [ (connectIrc       , (addIrc        cIrc       , runBot =<< ircBot))
    , (connectMatrix    , (addMatrix     cMatrix    , runBot =<< matrixBot))
    , (connectMattermost, (addMattermost cMattermost, runBot =<< mattermostBot))
    , (connectTelegram  , (addTelegram   cTelegram  , runBot =<< telegramBot))
    , (connectXmpp      , (addXmpp       cXmpp      , runBot =<< xmppBot))
    ]

addIrc :: IrcC -> Severity -> Env m -> IO (Env m)
addIrc IrcC {..} s env = do
  logIO s I "IRC" "connecting..."
  ircSocket <- ircConnect ircHost ircPort ircChan ircNick ircName
  logIO s I "IRC" "connected"
  pure env { envIrc = Irc {..} }

addMatrix :: MatrixC -> Severity -> Env m -> IO (Env m)
addMatrix MatrixC {..} s env = do
  maSinceR <- newIORef $ if T.null maSince then Nothing else Just maSince
  logIO s I "Matrix" "ready to go"
  pure env { envMatrix = Matrix {..} }

addMattermost :: MattermostC -> Severity -> Env m -> IO (Env m)
addMattermost MattermostC {..} s env = do
  logIO s I "Mattermost" "connecting..."
  mm <- mattermostConnect mmHost mmPort mmPath mmNick mmPswd
  logIO s I "Mattermost" "connected"
  pure env { envMattermost = mm }

addTelegram :: TelegramC -> Severity -> Env m -> IO (Env m)
addTelegram TelegramC {..} s env = do
  tgOffsetR <- newIORef tgOffset
  logIO s I "Telegram" "ready to go"
  pure env { envTelegram = Telegram {..} }

addXmpp :: XmppC -> Severity -> Env m -> IO (Env m)
addXmpp XmppC {..} s env = do
  logIO s I "XMPP" "connecting..."
  xmpp <- xmppConnect xmppHost xmppNick xmppPswd
  logIO s I "XMPP" "connected"
  pure env { envXmpp = xmpp }

main :: IO ()
main = do
  (env, apps) <- mkEnvAndApps =<< loadConfig
  runApp env $ runConcurrently $ foldr1 (*>) $ Concurrently <$> apps

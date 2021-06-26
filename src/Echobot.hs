{-# OPTIONS -Wno-missing-fields #-}

module Echobot
  ( main
  )
where

import           Data.List                      ( foldr1 )
import           Echobot.App.Env                ( Env(..) )
import           Echobot.App.Monad              ( AppEnv
                                                , runApp
                                                )
import           Echobot.Bots.Irc               ( ircBot )
import           Echobot.Bots.Matrix            ( matrixBot )
import           Echobot.Bots.Mattermost        ( mattermostBot )
import           Echobot.Bots.Telegram          ( telegramBot )
import           Echobot.Bots.Xmpp              ( xmppBot )
import           Echobot.Config                 ( Config(..)
                                                , loadConfig
                                                )
import           Echobot.Connectors.Irc         ( ircConnect )
import           Echobot.Connectors.Mattermost  ( mattermostConnect )
import           Echobot.Connectors.Xmpp        ( xmppConnect )
import           Echobot.Runner                 ( botRunner )
import           Echobot.Log                    ( logIO )
import           Echobot.Types.Severity         ( Severity(..) )
import           Echobot.Types.ToConnect        ( ToConnect(..) )
import           Echobot.Types.Irc              ( Irc(..)
                                                , IrcC(..)
                                                )
import           Echobot.Types.Matrix           ( Matrix(..)
                                                , MatrixC(..)
                                                )
import           Echobot.Types.Mattermost       ( MattermostC(..) )
import           Echobot.Types.Telegram         ( Telegram(..)
                                                , TelegramC(..)
                                                )
import           Echobot.Types.Xmpp             ( XmppC(..) )
import           UnliftIO.Async                 ( Concurrently(..)
                                                , runConcurrently
                                                )

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config { cConnect = ToConnect {..}, ..} = foldr1
  (>=>)
  (snd <$> filter fst
    [ (connectIrc       , addIrc        cIrc)
    , (connectMatrix    , addMatrix     cMatrix)
    , (connectMattermost, addMattermost cMattermost)
    , (connectTelegram  , addTelegram   cTelegram)
    , (connectXmpp      , addXmpp       cXmpp)
    ]
  )
  (Env { envSeverity = cSeverity, envDflts = cDflts, envMsgs = cMsgs })

addIrc :: IrcC -> Env m -> IO (Env m)
addIrc IrcC {..} env = do
  logIO N "IRC" "connecting..."
  h <- ircConnect cIrcHost cIrcPort cIrcChan cIrcNick cIrcName
  logIO N "IRC" "connected"
  pure env { envIrc = Irc h cIrcChan }

addMatrix :: MatrixC -> Env m -> IO (Env m)
addMatrix MatrixC {..} env = do
  since <- newIORef $ if cMSince == "" then Nothing else Just cMSince
  logIO N "Matrix" "ready to go"
  pure env { envMatrix = Matrix cMToken cMName cMHomeserver since }

addMattermost :: MattermostC -> Env m -> IO (Env m)
addMattermost MattermostC {..} env = do
  logIO N "Mattermost" "connecting..."
  mm <- mattermostConnect cMmHost cMmPort cMmPath cMmNick cMmPswd
  logIO N "Mattermost" "connected"
  pure env { envMattermost = mm }

addTelegram :: TelegramC -> Env m -> IO (Env m)
addTelegram TelegramC {..} env = do
  o <- newIORef cTgOffset
  logIO N "Telegram" "ready to go"
  pure env { envTelegram = Telegram cTgToken o }

addXmpp :: XmppC -> Env m -> IO (Env m)
addXmpp XmppC {..} env = do
  logIO N "XMPP" "connecting..."
  xmpp <- xmppConnect cXmppHost cXmppNick cXmppPswd
  logIO N "XMPP" "connected"
  pure env { envXmpp = xmpp }

runBots :: AppEnv -> ToConnect -> IO ()
runBots env ToConnect {..} = runApp env $ do
  let actions = snd <$> filter fst
        [ (connectIrc       , botRunner =<< ircBot)
        , (connectMatrix    , botRunner =<< matrixBot)
        , (connectMattermost, botRunner =<< mattermostBot)
        , (connectTelegram  , botRunner =<< telegramBot)
        , (connectXmpp      , botRunner =<< xmppBot)
        ]
  runConcurrently $ foldr1 (*>) $ Concurrently <$> actions

main :: IO ()
main = do
  conf   <- loadConfig
  appEnv <- mkAppEnv conf
  runBots appEnv $ cConnect conf

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
  ((`snd` cSeverity) <$> filter fst
    [ (connectIrc       , addIrc        cIrc)
    , (connectMatrix    , addMatrix     cMatrix)
    , (connectMattermost, addMattermost cMattermost)
    , (connectTelegram  , addTelegram   cTelegram)
    , (connectXmpp      , addXmpp       cXmpp)
    ]
  )
  (Env { envSeverity = cSeverity, envDflts = cDflts, envMsgs = cMsgs })

addIrc :: IrcC -> Severity -> Env m -> IO (Env m)
addIrc IrcC {..} s env = do
  logIO s I "IRC" "connecting..."
  h <- ircConnect cIrcHost cIrcPort cIrcChan cIrcNick cIrcName
  logIO s I "IRC" "connected"
  pure env { envIrc = Irc h cIrcChan }

addMatrix :: MatrixC -> Severity -> Env m -> IO (Env m)
addMatrix MatrixC {..} s env = do
  since <- newIORef $ if cMSince == "" then Nothing else Just cMSince
  logIO s I "Matrix" "ready to go"
  pure env { envMatrix = Matrix cMToken cMName cMHomeserver since }

addMattermost :: MattermostC -> Severity -> Env m -> IO (Env m)
addMattermost MattermostC {..} s env = do
  logIO s I "Mattermost" "connecting..."
  mm <- mattermostConnect cMmHost cMmPort cMmPath cMmNick cMmPswd
  logIO s I "Mattermost" "connected"
  pure env { envMattermost = mm }

addTelegram :: TelegramC -> Severity -> Env m -> IO (Env m)
addTelegram TelegramC {..} s env = do
  o <- newIORef cTgOffset
  logIO s I "Telegram" "ready to go"
  pure env { envTelegram = Telegram cTgToken o }

addXmpp :: XmppC -> Severity -> Env m -> IO (Env m)
addXmpp XmppC {..} s env = do
  logIO s I "XMPP" "connecting..."
  xmpp <- xmppConnect cXmppHost cXmppNick cXmppPswd
  logIO s I "XMPP" "connected"
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

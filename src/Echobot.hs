{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Echobot
  ( main
  )
where

import           Colog                          ( filterBySeverity
                                                , msgSeverity
                                                , richMessageAction
                                                )
import           Data.List                      ( foldr1 )
import           Echobot.App.Env                ( Env(..)
                                                , initialisedField
                                                )
import           Echobot.App.Monad              ( AppEnv
                                                , runApp
                                                )
import           Echobot.Core.Irc               ( Irc(..)
                                                , IrcC(..)
                                                )
import           Echobot.Core.ToConnect         ( ToConnect(..) )
import           Echobot.Core.Telegram          ( Telegram(..)
                                                , TelegramC(..)
                                                )
import           Echobot.Core.Matrix            ( Matrix(..)
                                                , MatrixC(..)
                                                )
import           Echobot.Core.Mattermost        ( Mattermost(..)
                                                , MattermostC(..)
                                                )
import           Echobot.Core.Xmpp              ( Xmpp(..)
                                                , XmppC(..)
                                                )
import           Echobot.Config                 ( Config(..)
                                                , loadConfig
                                                )
import           Echobot.Connectors.Handle      ( handleConnect )
import           Echobot.Connectors.Xmpp        ( xmppConnect )
import           Echobot.Connectors.Mattermost  ( mattermostConnect )
import           Echobot.Runner                 ( botRunner )
import           Echobot.Bots.Irc               ( ircBot )
import           Echobot.Bots.Telegram          ( telegramBot )
import           Echobot.Bots.Matrix            ( matrixBot )
import           Echobot.Bots.Mattermost        ( mattermostBot )
import           Echobot.Bots.Xmpp              ( xmppBot )
import           UnliftIO.Async                 ( Concurrently(..)
                                                , runConcurrently
                                                )

mkAppEnv :: Config -> IO AppEnv
mkAppEnv c@Config {..} =
  pure Env
    { envLogAction = filterBySeverity cLogSeverity msgSeverity richMessageAction
    , envDflts     = cDflts
    , envMsgs      = cMsgs
    }
  >>= (if connectIrc        cConnect then addIrc        c else pure)
  >>= (if connectMatrix     cConnect then addMatrix     c else pure)
  >>= (if connectMattermost cConnect then addMattermost c else pure)
  >>= (if connectTelegram   cConnect then addTelegram   c else pure)
  >>= (if connectXmpp       cConnect then addXmpp       c else pure)

addIrc :: Config -> Env m -> IO (Env m)
addIrc Config {..} env = do
  irc <- do
    putTextLn "[IRC] connecting..."
    h <- handleConnect (cIrcHost cIrc)
                       (cIrcPort cIrc)
    putTextLn "[IRC] connected"
    pure $ Irc h (cIrcChan cIrc)
                 (cIrcNick cIrc)
                 (cIrcName cIrc)
  pure env { envIrc = irc }

addMatrix :: Config -> Env m -> IO (Env m)
addMatrix Config {..} env = do
  matrix <- do
    s <- newIORef
      $ if cMSince cMatrix == "" then Nothing else Just $ cMSince cMatrix
    putTextLn "[Matrix] ready to go"
    pure $ Matrix s (cMToken      cMatrix)
                    (cMName       cMatrix)
                    (cMHomeserver cMatrix)
  pure env { envMatrix = matrix }

addMattermost :: Config -> Env m -> IO (Env m)
addMattermost Config {..} env = do
  mm <- do
    putTextLn "[Mattermost] connecting..."
    s <- mattermostConnect (cMmHost cMattermost)
                           (cMmPort cMattermost)
                           (cMmPath cMattermost)
                           (cMmNick cMattermost)
                           (cMmPswd cMattermost)
    putTextLn "[Mattermost] connected"
    pure $ Mattermost s
  pure env { envMattermost = mm }

addTelegram :: Config -> Env m -> IO (Env m)
addTelegram Config {..} env = do
  tg <- do
    o <- newIORef $ cTgOffset cTelegram
    putTextLn "[Telegram] ready to go"
    pure $ Telegram o (cTgToken cTelegram)
  pure env { envTelegram = tg }

addXmpp :: Config -> Env m -> IO (Env m)
addXmpp Config {..} env = do
  xmpp <- do
    putTextLn "[XMPP] connecting..."
    s <- xmppConnect (cXmppHost cXmpp)
                     (cXmppNick cXmpp)
                     (cXmppPswd cXmpp)
    putTextLn "[XMPP] connected"
    pure $ Xmpp s
  pure env { envXmpp = xmpp }

runBots :: AppEnv -> IO ()
runBots = flip runApp $ do
  i  <- initialisedField @Irc
  m  <- initialisedField @Matrix
  mm <- initialisedField @Mattermost
  tg <- initialisedField @Telegram
  x  <- initialisedField @Xmpp
  let actions = catMaybes
        [ if i  then Just $ botRunner =<< ircBot        else Nothing
        , if m  then Just $ botRunner =<< matrixBot     else Nothing
        , if mm then Just $ botRunner =<< mattermostBot else Nothing
        , if tg then Just $ botRunner =<< telegramBot   else Nothing
        , if x  then Just $ botRunner =<< xmppBot       else Nothing
        ]
  runConcurrently $ foldr1 (*>) $ Concurrently <$> actions

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runBots

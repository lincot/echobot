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
import           Data.List                      ( foldl1 )
import           Echobot.App.Env                ( Env(..)
                                                , hasField'
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
  >>= (if connectIrc        cConnect then addIrc        c else return)
  >>= (if connectMatrix     cConnect then addMatrix     c else return)
  >>= (if connectMattermost cConnect then addMattermost c else return)
  >>= (if connectTelegram   cConnect then addTelegram   c else return)
  >>= (if connectXmpp       cConnect then addXmpp       c else return)

addIrc :: Config -> Env m -> IO (Env m)
addIrc Config {..} env = do
  irc <- do
    putTextLn "[IRC] connecting..."
    h <- handleConnect (cIrcHost cIrc)
                       (cIrcPort cIrc)
    putTextLn "[IRC] connected"
    return $ Irc h (cIrcChan cIrc)
                   (cIrcNick cIrc)
                   (cIrcName cIrc)
  return env { envIrc = irc }

addMatrix :: Config -> Env m -> IO (Env m)
addMatrix Config {..} env = do
  matrix <- do
    s <- newIORef
      $ if cMSince cMatrix == "" then Nothing else Just $ cMSince cMatrix
    putTextLn "[Matrix] ready to go"
    return $ Matrix s (cMToken      cMatrix)
                      (cMName       cMatrix)
                      (cMHomeserver cMatrix)
  return env { envMatrix = matrix }

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
    return $ Mattermost s
  return env { envMattermost = mm }

addTelegram :: Config -> Env m -> IO (Env m)
addTelegram Config {..} env = do
  tg <- do
    o <- newIORef $ cTgOffset cTelegram
    putTextLn "[Telegram] ready to go"
    return $ Telegram o (cTgToken cTelegram)
  return env { envTelegram = tg }

addXmpp :: Config -> Env m -> IO (Env m)
addXmpp Config {..} env = do
  xmpp <- do
    putTextLn "[XMPP] connecting..."
    s <- xmppConnect (cXmppHost cXmpp)
                     (cXmppNick cXmpp)
                     (cXmppPswd cXmpp)
    putTextLn "[XMPP] connected"
    return $ Xmpp s
  return env { envXmpp = xmpp }

runBots :: AppEnv -> IO ()
runBots env = runApp env $ do
  i  <- hasField' @Irc
  m  <- hasField' @Matrix
  mm <- hasField' @Mattermost
  tg <- hasField' @Telegram
  x  <- hasField' @Xmpp
  let actions = catMaybes
        [ if i  then Just $ botRunner =<< ircBot        else Nothing
        , if m  then Just $ botRunner =<< matrixBot     else Nothing
        , if mm then Just $ botRunner =<< mattermostBot else Nothing
        , if tg then Just $ botRunner =<< telegramBot   else Nothing
        , if x  then Just $ botRunner =<< xmppBot       else Nothing
        ]
  runConcurrently $ foldl1 (*>) $ Concurrently <$> actions

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runBots

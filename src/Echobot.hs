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
import           Echobot.Types.Irc              ( Irc(..)
                                                , IrcC(..)
                                                )
import           Echobot.Types.ToConnect        ( ToConnect(..) )
import           Echobot.Types.Telegram         ( Telegram(..)
                                                , TelegramC(..)
                                                )
import           Echobot.Types.Matrix           ( Matrix(..)
                                                , MatrixC(..)
                                                )
import           Echobot.Types.Mattermost       ( Mattermost
                                                , MattermostC(..)
                                                )
import           Echobot.Types.Xmpp             ( Xmpp
                                                , XmppC(..)
                                                )
import           Echobot.Config                 ( Config(..)
                                                , loadConfig
                                                )
import           Echobot.Connectors.Irc         ( ircConnect )
import           Echobot.Connectors.Mattermost  ( mattermostConnect )
import           Echobot.Connectors.Xmpp        ( xmppConnect )
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
mkAppEnv conf@Config {..} =
  pure Env
      { envLogAction = filterBySeverity cLogSeverity
                                        msgSeverity
                                        richMessageAction
      , envDflts     = cDflts
      , envMsgs      = cMsgs
      }
    >>= foldr1
          (>=>)
          ((`snd` conf) <$> filter (`fst` cConnect)
            [ (connectIrc       , addIrc)
            , (connectMatrix    , addMatrix)
            , (connectMattermost, addMattermost)
            , (connectTelegram  , addTelegram)
            , (connectXmpp      , addXmpp)
            ]
          )

addIrc :: Config -> Env m -> IO (Env m)
addIrc Config {..} env = do
  putTextLn "[IRC] connecting..."
  h <- ircConnect (cIrcHost cIrc)
                  (cIrcPort cIrc)
                  (cIrcChan cIrc)
                  (cIrcNick cIrc)
                  (cIrcName cIrc)
  putTextLn "[IRC] connected"
  pure env { envIrc = Irc h (cIrcChan cIrc) }

addMatrix :: Config -> Env m -> IO (Env m)
addMatrix Config {..} env = do
  since <- newIORef
    $ if cMSince cMatrix == "" then Nothing else Just $ cMSince cMatrix
  putTextLn "[Matrix] ready to go"
  pure env
    { envMatrix = Matrix (cMToken      cMatrix)
                         (cMName       cMatrix)
                         (cMHomeserver cMatrix)
                         since
    }

addMattermost :: Config -> Env m -> IO (Env m)
addMattermost Config {..} env = do
  putTextLn "[Mattermost] connecting..."
  mm <- mattermostConnect (cMmHost cMattermost)
                          (cMmPort cMattermost)
                          (cMmPath cMattermost)
                          (cMmNick cMattermost)
                          (cMmPswd cMattermost)
  putTextLn "[Mattermost] connected"
  pure env { envMattermost = mm }

addTelegram :: Config -> Env m -> IO (Env m)
addTelegram Config {..} env = do
  o <- newIORef $ cTgOffset cTelegram
  putTextLn "[Telegram] ready to go"
  pure env { envTelegram = Telegram (cTgToken cTelegram) o }

addXmpp :: Config -> Env m -> IO (Env m)
addXmpp Config {..} env = do
  putTextLn "[XMPP] connecting..."
  xmpp <- xmppConnect (cXmppHost cXmpp) (cXmppNick cXmpp) (cXmppPswd cXmpp)
  putTextLn "[XMPP] connected"
  pure env { envXmpp = xmpp }

runBots :: AppEnv -> IO ()
runBots = flip runApp $ do
  i  <- initialisedField @Irc
  m  <- initialisedField @Matrix
  mm <- initialisedField @Mattermost
  tg <- initialisedField @Telegram
  x  <- initialisedField @Xmpp
  let actions = snd <$> filter fst
        [ (i , botRunner =<< ircBot)
        , (m , botRunner =<< matrixBot)
        , (mm, botRunner =<< mattermostBot)
        , (tg, botRunner =<< telegramBot)
        , (x , botRunner =<< xmppBot)
        ]
  runConcurrently $ foldr1 (*>) $ Concurrently <$> actions

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runBots

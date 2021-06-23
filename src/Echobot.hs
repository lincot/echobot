{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards    #-}

module Echobot
  ( main
  )
where

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
mkAppEnv Config {..} =
  pure Env { envSeverity = cSeverity, envDflts = cDflts, envMsgs = cMsgs }
    >>= foldr1
          (>=>)
          (snd <$> filter (`fst` cConnect)
            [ (connectIrc       , addIrc        cIrc)
            , (connectMatrix    , addMatrix     cMatrix)
            , (connectMattermost, addMattermost cMattermost)
            , (connectTelegram  , addTelegram   cTelegram)
            , (connectXmpp      , addXmpp       cXmpp)
            ]
          )

addIrc :: IrcC -> Env m -> IO (Env m)
addIrc IrcC {..} env = do
  putTextLn "[IRC] connecting..."
  h <- ircConnect cIrcHost cIrcPort cIrcChan cIrcNick cIrcName
  putTextLn "[IRC] connected"
  pure env { envIrc = Irc h cIrcChan }

addMatrix :: MatrixC -> Env m -> IO (Env m)
addMatrix MatrixC {..} env = do
  since <- newIORef $ if cMSince == "" then Nothing else Just cMSince
  putTextLn "[Matrix] ready to go"
  pure env { envMatrix = Matrix cMToken cMName cMHomeserver since }

addMattermost :: MattermostC -> Env m -> IO (Env m)
addMattermost MattermostC {..} env = do
  putTextLn "[Mattermost] connecting..."
  mm <- mattermostConnect cMmHost cMmPort cMmPath cMmNick cMmPswd
  putTextLn "[Mattermost] connected"
  pure env { envMattermost = mm }

addTelegram :: TelegramC -> Env m -> IO (Env m)
addTelegram TelegramC {..} env = do
  o <- newIORef cTgOffset
  putTextLn "[Telegram] ready to go"
  pure env { envTelegram = Telegram cTgToken o }

addXmpp :: XmppC -> Env m -> IO (Env m)
addXmpp XmppC {..} env = do
  putTextLn "[XMPP] connecting..."
  xmpp <- xmppConnect cXmppHost cXmppNick cXmppPswd
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

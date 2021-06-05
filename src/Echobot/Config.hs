module Echobot.Config
  ( Config(..)
  , loadConfig
  )
where

import           Colog                          ( Severity(..) )
import           Echobot.Core.Dflts             ( Dflts(..) )
import           Echobot.Core.Irc               ( IrcC(..) )
import           Echobot.Core.Msgs              ( Msgs(..) )
import           Echobot.Core.Matrix            ( MatrixC(..) )
import           Echobot.Core.Mattermost        ( MattermostC(..) )
import           Echobot.Core.Telegram          ( TelegramC(..) )
import           Echobot.Core.ToConnect         ( ToConnect(..) )
import           Echobot.Core.Xmpp              ( XmppC(..) )
import           Toml                           ( TomlCodec
                                                , (.=)
                                                )
import qualified Toml

data Config = Config
  { cLogSeverity :: !Severity
  , cConnect     :: !ToConnect
  , cIrc         :: !IrcC
  , cMatrix      :: !MatrixC
  , cMattermost  :: !MattermostC
  , cTelegram    :: !TelegramC
  , cXmpp        :: !XmppC
  , cDflts       :: !Dflts
  , cMsgs        :: !Msgs
  }

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.read               "log.severity" .= cLogSeverity
  <*> Toml.table connectCodec "connect"      .= cConnect
  <*> Toml.table ircCodec     "IRC"          .= cIrc
  <*> Toml.table matrixCodec  "Matrix"       .= cMatrix
  <*> Toml.table mmCodec      "Mattermost"   .= cMattermost
  <*> Toml.table tgCodec      "Telegram"     .= cTelegram
  <*> Toml.table xmppCodec    "XMPP"         .= cXmpp
  <*> Toml.table dfltsCodec   "defaults"     .= cDflts
  <*> Toml.table msgsCodec    "messages"     .= cMsgs

connectCodec :: TomlCodec ToConnect
connectCodec = ToConnect
  <$> Toml.bool "IRC"        .= connectIrc
  <*> Toml.bool "Matrix"     .= connectMattermost
  <*> Toml.bool "Mattermost" .= connectMattermost
  <*> Toml.bool "Telegram"   .= connectTelegram
  <*> Toml.bool "XMPP"       .= connectXmpp

ircCodec :: TomlCodec IrcC
ircCodec = IrcC
  <$> Toml.string "host" .= cIrcHost
  <*> Toml.string "port" .= cIrcPort
  <*> Toml.text   "chan" .= cIrcChan
  <*> Toml.text   "nick" .= cIrcNick
  <*> Toml.text   "name" .= cIrcName

matrixCodec :: TomlCodec MatrixC
matrixCodec = MatrixC
  <$> Toml.text "token"      .= cMToken
  <*> Toml.text "name"       .= cMName
  <*> Toml.text "homeserver" .= cMHomeserver
  <*> Toml.text "since"      .= cMSince

mmCodec :: TomlCodec MattermostC
mmCodec = MattermostC
  <$> Toml.text "host"     .= cMmHost
  <*> Toml.int  "port"     .= cMmPort
  <*> Toml.text "path"     .= cMmPath
  <*> Toml.text "nick"     .= cMmNick
  <*> Toml.text "password" .= cMmPswd

tgCodec :: TomlCodec TelegramC
tgCodec = TelegramC
  <$> Toml.text "token"  .= cTgToken
  <*> Toml.int  "offset" .= cTgOffset

xmppCodec :: TomlCodec XmppC
xmppCodec = XmppC
  <$> Toml.string "host"     .= cXmppHost
  <*> Toml.text   "nick"     .= cXmppNick
  <*> Toml.text   "password" .= cXmppPswd

dfltsCodec :: TomlCodec Dflts
dfltsCodec = Dflts
  <$> Toml.int "repeatCount" .= defaultRepeatCount

msgsCodec :: TomlCodec Msgs
msgsCodec = Msgs
  <$> Toml.text "help"    .= helpMsg
  <*> Toml.text "repeat"  .= repeatMsg
  <*> Toml.text "invalid" .= invalidMsg

loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configCodec "config.toml"

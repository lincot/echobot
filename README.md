# echobot

a chat bot for IRC/Matrix/Mattermost/Telegram/XMPP in Haskell

with Matrix only unencrypted rooms work

external deps:

- `icu` for `pontarius-xmpp`

to build, use `stack build`

to run, edit `config.toml`, then `stack run`

## design

config gets read in `Echobot.Config`, then turned into an `AppEnv` in `Echobot`

bots' behavior is in `Echobot.Runner`

the bots for different services are `Echobot.Bots.*`

# echobot

a chat bot for IRC/Matrix/Mattermost/Telegram/XMPP in Haskell

with Matrix only unencrypted rooms work

# external deps

C: `icu` (for `pontarius-xmpp`)

# build

`stack build`/`cabal build`

# run

edit `config.toml` and run `stack run`/`cabal run`

# design

config gets read in `Echobot.Config`, then turned into an `AppEnv` in `Echobot`

bots' behavior is in `Echobot.Runner`

the bots for different services are `Echobot.Bots.*`

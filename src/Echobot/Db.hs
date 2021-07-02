module Echobot.Db
  ( getUser
  , putUser
  )
where

import           Echobot.App.Monad              ( App )
import           Echobot.Types.Users            ( User
                                                , Users
                                                )

getUser :: (Eq u, Hashable u) => Users u -> u -> App (Maybe User)
getUser users uid = (!? uid) <$> readIORef users

putUser :: (Eq u, Hashable u) => Users u -> u -> User -> App ()
putUser users uid user = modifyIORef' users $ insert uid user

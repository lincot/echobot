module Echobot.Db
  ( getUser
  , putUser
  ) where

import           Echobot.App.Monad              ( App )
import           Echobot.Types.User             ( User
                                                , UsersR
                                                )

getUser :: (Eq u, Hashable u) => UsersR u -> u -> App (Maybe User)
getUser usersR uid = (!? uid) <$> readIORef usersR

putUser :: (Eq u, Hashable u) => UsersR u -> u -> User -> App ()
putUser usersR uid user = modifyIORef' usersR $ insert uid user

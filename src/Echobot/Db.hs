module Echobot.Db
  ( getUser
  , putUser
  )
where

import qualified Data.HashMap.Strict           as HM
import           Echobot.Core.Users             ( Users
                                                , User
                                                )
import           Echobot.App.Monad              ( App )

getUser :: (Eq u, Hashable u) => Users u -> u -> App (Maybe User)
getUser usrs name = do
  usersMap <- readIORef usrs
  pure $ HM.lookup name usersMap

putUser :: (Eq u, Hashable u) => Users u -> u -> User -> App ()
putUser usrs name user = modifyIORef' usrs $ HM.insert name user

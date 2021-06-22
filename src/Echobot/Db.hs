module Echobot.Db
  ( getUser
  , putUser
  )
where

import qualified Data.HashMap.Strict           as HM
import           Echobot.Types.Users            ( Users
                                                , User
                                                )
import           Echobot.App.Monad              ( App )

getUser :: (Eq u, Hashable u) => Users u -> u -> App (Maybe User)
getUser usrs name = HM.lookup name <$> readIORef usrs

putUser :: (Eq u, Hashable u) => Users u -> u -> User -> App ()
putUser usrs name user = modifyIORef' usrs $ HM.insert name user

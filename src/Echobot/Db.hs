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
getUser users uid = HM.lookup uid <$> readIORef users

putUser :: (Eq u, Hashable u) => Users u -> u -> User -> App ()
putUser users uid user = modifyIORef' users $ HM.insert uid user

module Echobot.Core.Matrix
  ( Matrix(..)
  , MatrixC(..)
  )
where

data Matrix = Matrix
  { mToken      :: !Text
  , mName       :: !Text
  , mHomeserver :: !Text
  , mSince      :: !(IORef (Maybe Text))
  }

data MatrixC = MatrixC
  { cMToken      :: !Text
  , cMName       :: !Text
  , cMHomeserver :: !Text
  , cMSince      :: !Text
  }

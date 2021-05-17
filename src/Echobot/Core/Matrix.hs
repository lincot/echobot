module Echobot.Core.Matrix
  ( Matrix(..)
  , MatrixC(..)
  )
where

data Matrix = Matrix
  { mSince      :: !(IORef (Maybe Text))
  , mToken      :: !Text
  , mName       :: !Text
  , mHomeserver :: !Text
  }

data MatrixC = MatrixC
  { cMSince      :: !Text
  , cMToken      :: !Text
  , cMName       :: !Text
  , cMHomeserver :: !Text
  }

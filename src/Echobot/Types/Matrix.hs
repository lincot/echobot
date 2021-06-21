module Echobot.Types.Matrix
  ( Matrix(..)
  , MatrixC(..)
  )
where

data Matrix = Matrix
  { mToken, mName, mHomeserver :: !Text
  , mSince :: !(IORef (Maybe Text))
  }

data MatrixC = MatrixC
  { cMToken, cMName, cMHomeserver, cMSince :: !Text
  }

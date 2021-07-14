module Echobot.Types.Matrix
  ( Matrix(..)
  , MatrixC(..)
  ) where

data Matrix = Matrix
  { maToken, maName, maHomeserver :: !Text
  , maSinceR                      :: !(IORef (Maybe Text))
  }

data MatrixC = MatrixC
  { maToken, maName, maHomeserver, maSince :: !Text
  }

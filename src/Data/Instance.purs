module Data.Instance
  ( InTheMirror
  ) where

import Prelude
import Data.Newtype (class Newtype)

-- TODO: ...any kind of class to tie this mess together...
-- like there's GOT to be some elegant way to like.
-- I don't even know. just encode superclass constraints in the first place LMAO
-- what was I just thinking about for the last 5 minutes
-- and I'm pretty sure it's fundamentally impossible to
-- make the InTheMirror instance a class thing

data InTheMirror r

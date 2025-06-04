module Data.Instance
  ( FunctorRep
  ) where

import Prelude

-- TODO: ...any kind of class to tie this mess together...
-- like there's GOT to be some elegant way to like.
-- I don't even know. just encode superclass constraints in the first place LMAO
-- what was I just thinking about for the last 5 minutes

newtype FunctorRep :: (Type -> Type) -> Type
newtype FunctorRep f = FunctorRep
  { map :: forall a b. (a -> b) -> a -> b
  }

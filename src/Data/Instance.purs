module Data.Instance
  ( FunctorInst(..)
  , functorInst
  ) where

import Prelude

-- TODO: ...any kind of class to tie this mess together...
-- like there's GOT to be some elegant way to like.
-- I don't even know. just encode superclass constraints in the first place LMAO
-- what was I just thinking about for the last 5 minutes

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

functorInst :: forall f. Functor f => FunctorInst f
functorInst = FunctorInst { map }

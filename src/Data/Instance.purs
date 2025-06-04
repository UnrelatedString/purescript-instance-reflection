module Data.Instance
  ( FunctorInst(..)
  , functorInst
  , ApplyInst(..)
  , ApplyInst'(..)
  , applyInst
  ) where

import Prelude
import Type.Proxy (Proxy)

-- TODO: ...any kind of class to tie this mess together...
-- like there's GOT to be some elegant way to like.
-- I don't even know. just encode superclass constraints in the first place LMAO
-- what was I just thinking about for the last 5 minutes

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

functorInst :: forall f. Functor f => Proxy f -> FunctorInst f
functorInst _ = FunctorInst { map }

data ApplyInst :: (Type -> Type) -> Type
data ApplyInst f = ApplyInst { "Functor" :: FunctorInst f } (ApplyInst' f)

newtype ApplyInst' f = ApplyInst'
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

applyInst :: forall f. Apply f => Proxy f -> ApplyInst f
applyInst p = ApplyInst { "Functor": functorInst p } $ ApplyInst' { apply }

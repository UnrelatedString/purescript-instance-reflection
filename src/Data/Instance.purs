module Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , FunctorInst(..)
  , ApplyInst(..)
  ) where

import Prelude
import Type.Proxy (Proxy)

class ReflectInstance :: forall k. (k -> Type) -> k -> Constraint
class ReflectInstance reflection for | reflection -> for where
  reflectInstance :: reflection for

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance Functor f => ReflectInstance FunctorInst f where
  reflectInstance = FunctorInst { map }

newtype ApplyInst f = ApplyInst
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance Apply f => ReflectInstance ApplyInst f where
  reflectInstance = ApplyInst { apply }

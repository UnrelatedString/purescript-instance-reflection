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

class Super :: forall k. (k -> Type) -> (k -> Type) -> Constraint
class Super r s where
  immediateSuper :: forall a. r a -> s a

data FunctorInst :: (Type -> Type) -> Type
data FunctorInst f = FunctorInst {}
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance Functor f => ReflectInstance FunctorInst f where
  reflectInstance = FunctorInst {} { map }

data ApplyInst :: (Type -> Type) -> Type
data ApplyInst f = ApplyInst { "Functor" :: FunctorInst f }
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance Apply f => ReflectInstance ApplyInst f where
  reflectInstance = ApplyInst { "Functor": reflectInstance } { apply }

instance Super ApplyInst FunctorInst where
  immediateSuper (ApplyInst { "Functor": s} _) = s

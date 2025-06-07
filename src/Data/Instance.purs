module Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , reflectFromNewtype
  , AnySuper
  , class ProvidesAnySuper
  , anySuper
  , FunctorInst(..)
  , ApplyInst(..)
  ) where

import Prelude
import Safe.Coerce (class Coercible, coerce)
import Type.Proxy (Proxy)

class ReflectInstance :: forall k. (k -> Type) -> k -> Constraint
class ReflectInstance reflection for | reflection -> for where
  reflectInstance :: reflection for

-- | Reflect a newtype's instance as if it were an instance for the wrapped type.
reflectFromNewtype
  :: forall reflection for for'
  . ReflectInstance reflection for
  => Coercible (reflection for) (reflection for')
  => (for' -> for) -> reflection for'
reflectFromNewtype _ = coerce (reflectInstance :: reflection for)

type AnySuper :: forall k. (k -> Type) -> k -> Type
type AnySuper reflection for
  = ReflectInstance reflection for
  => (forall reflection'. ReflectInstance reflection' for => reflection' for)

class ProvidesAnySuper :: forall k. Type -> (k -> Type) -> k -> Constraint
class ProvidesAnySuper a reflection for where
  anySuper :: a -> AnySuper reflection for

instance ProvidesAnySuper (Proxy (reflection for)) reflection for where
  anySuper _ = reflectInstance

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance Functor f => ReflectInstance FunctorInst f where
  reflectInstance = FunctorInst { map }

newtype ApplyInst :: (Type -> Type) -> Type
newtype ApplyInst f = ApplyInst
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance (Apply f, ReflectInstance FunctorInst f) => ReflectInstance ApplyInst f where
  reflectInstance = ApplyInst { apply }

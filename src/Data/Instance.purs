module Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , reflectFromNewtype
  , AnySuper
  , reflectAnySuper
  , FunctorInst(..)
  , ApplyInst(..)
  ) where

import Prelude
import Safe.Coerce (class Coercible, coerce)
import Type.Proxy (Proxy)

class ReflectInstance :: forall k. (k -> Type) -> k -> Constraint
class ReflectInstance reflection for where
  reflectInstance :: reflection for

-- | Reflect a newtype's instance as if it were an instance for the wrapped type.
reflectFromNewtype
  :: forall reflection for for'
  . ReflectInstance reflection for
  => Coercible (reflection for) (reflection for')
  => (for' -> for) -> reflection for'
reflectFromNewtype _ = coerce (reflectInstance :: reflection for)

type AnySuper :: forall k. (k -> Type) -> Type
type AnySuper reflection = forall for. ReflectInstance reflection for => forall super. ReflectInstance super for => super for

reflectAnySuper :: forall for. Proxy for -> forall reflection. AnySuper reflection
reflectAnySuper _ = reflectInstance

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

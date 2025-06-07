module Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , reflectFromNewtype
  , FunctorInst(..)
  , ApplyInst(..)
  ) where

import Prelude
import Prim.Row (class Cons)
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

class Has row t
instance Cons sym t row row' => Has row t

class Satisfiable :: forall k. (k -> Type) -> Row (k -> Type) -> Constraint
class Has row reflection <= Satisfiable reflection row

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance Functor f => ReflectInstance FunctorInst f where
  reflectInstance = FunctorInst { map }

instance Has r FunctorInst => Satisfiable FunctorInst r

newtype ApplyInst :: (Type -> Type) -> Type
newtype ApplyInst f = ApplyInst
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance Apply f => ReflectInstance ApplyInst f where
  reflectInstance = ApplyInst { apply }

instance (Has r ApplyInst, Satisfiable FunctorInst r) => Satisfiable ApplyInst r

module Data.Instance
  ( class ReflectInstance
  , class HasSupers
  , reflectInstance
  , reflectFromNewtype
  , EqInst(..)
  , Eq1Inst(..)
  , FunctorInst(..)
  , ApplyInst(..)
  , ApplicativeInst(..)
  ) where

import Prelude
import Data.Eq (class Eq1, eq1)
import Safe.Coerce (class Coercible, coerce)
import Type.Proxy (Proxy(..))

class HasSupers :: forall k. Type -> (k -> Type) -> k -> Constraint
class HasSupers via reflection for | via -> for

class ReflectInstance :: forall k. Type -> (k -> Type) -> k -> Constraint
class HasSupers via reflection for <= ReflectInstance via reflection for | via -> for where
  reflectInstance :: via -> reflection for

-- | Reflect a newtype's instance as if it were an instance for the wrapped type.
reflectFromNewtype
  :: forall reflection for for'
  . ReflectInstance (Proxy for) reflection for
  => Coercible (reflection for) (reflection for')
  => (for' -> for) ->  reflection for'
reflectFromNewtype _ = coerce (reflectInstance (Proxy @for) :: reflection for)

newtype EqInst :: Type -> Type
newtype EqInst a = EqInst
  { eq :: a -> a -> Boolean
  }

instance HasSupers via EqInst a
instance Eq a => ReflectInstance (Proxy a) EqInst a where
  reflectInstance _ = EqInst { eq }

newtype Eq1Inst :: (Type -> Type) -> Type
newtype Eq1Inst f = Eq1Inst
  { eq1 :: forall a. Eq a => f a -> f a -> Boolean
  }

instance HasSupers via Eq1Inst f
instance Eq1 f => ReflectInstance (Proxy f) Eq1Inst f where
  reflectInstance _ = Eq1Inst { eq1 }

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance HasSupers via FunctorInst f
instance Functor f => ReflectInstance (Proxy f) FunctorInst f where
  reflectInstance _ = FunctorInst { map }

newtype ApplyInst :: (Type -> Type) -> Type
newtype ApplyInst f = ApplyInst
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance ReflectInstance via FunctorInst f => HasSupers via ApplyInst f
instance Apply f => ReflectInstance (Proxy f) ApplyInst f where
  reflectInstance _ = ApplyInst { apply }

newtype ApplicativeInst :: (Type -> Type) -> Type
newtype ApplicativeInst f = ApplicativeInst
  { pure :: forall a. a -> f a
  }

instance ReflectInstance via ApplyInst f => HasSupers via ApplicativeInst f
instance Applicative f => ReflectInstance (Proxy f) ApplicativeInst f where
  reflectInstance _ = ApplicativeInst { pure }

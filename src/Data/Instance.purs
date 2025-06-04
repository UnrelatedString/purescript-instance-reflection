module Data.Instance
  ( class ClassHierarchy
  , FunctorInst(..)
  , functorInst
  , ApplyInst(..)
  , applyInst
  ) where

import Prelude
import Type.Proxy (Proxy)

class ClassHierarchy :: forall k. k -> Symbol -> Row k -> Constraint
class ClassHierarchy inst reflects supers | inst -> reflects, inst -> supers

newtype FunctorInst :: (Type -> Type) -> Type
newtype FunctorInst f = FunctorInst
  { map :: forall a b. (a -> b) -> f a -> f b
  }

instance ClassHierarchy FunctorInst "Data.Functor (Functor)" ()

functorInst :: forall f. Functor f => FunctorInst f
functorInst = FunctorInst { map }

newtype ApplyInst f = ApplyInst'
  { apply :: forall a b. f (a -> b) -> f a -> f b
  }

instance ClassHierarchy ApplyInst "Data.Apply (Apply)"
  ( "Data.Functor (Functor)" :: FunctorInst
  )

applyInst :: forall f. Apply f => ApplyInst f
applyInst = ApplyInst { apply }

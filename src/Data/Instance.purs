module Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , FunctorInst(..)
  , ApplyInst(..)
  ) where

import Prelude
import Type.Proxy (Proxy)

-- | The kind of phantom types representing (Type -> Constraint)s which. actually does
-- | it even make sense to care about orphan stuff any more at this point when like
-- | you can easily homebrew this system anyways and this approach like very specifically
-- | treats the names of . actually no I should just go back to the main branch and
-- | make it build records of the newtypes like I was planning originally and ???
data

class InstanceFields

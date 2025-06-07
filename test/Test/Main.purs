module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (it)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)

import Data.Instance
  ( class ReflectInstance
  , reflectInstance
  , EqInst(..)
  , FunctorInst(..)
  , ApplyInst(..)
  , ApplicativeInst(..)
  )
import Type.Proxy (Proxy(..))

lift1FromProvider :: forall a b via for. ReflectInstance via ApplicativeInst for => via -> (a -> b) -> for a -> for b
lift1FromProvider = reflectInstance >>> \(FunctorInst {map: lift1}) -> lift1

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  it "all passes if this module even compiles" $ pure unit

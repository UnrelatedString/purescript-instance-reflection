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

pureFromProvider :: forall via for. ReflectInstance via ApplicativeInst for => via -> for Unit
pureFromProvider = reflectInstance >>> \(ApplicativeInst {pure: p}) -> p unit

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  it "all passes if this module even compiles" $ pure unit

module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (it)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)

import Data.Instance
  ( reflectInstance
  , EqInst(..)
  , FunctorInst(..)
  , ApplyInst(..)
  )
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  it "all passes if this module even compiles" $ pure unit

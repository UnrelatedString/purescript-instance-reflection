module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (it)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)

import Data.Instance
  ( reflectInstance
  , AnySuper
  , anySuper
  , FunctorInst(..)
  , ApplyInst(..)
  )
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  let arr = anySuper (Proxy@(ApplyInst Array)) :: AnySuper ApplyInst Array
  let _ = arr :: FunctorInst Array
  let _ = arr :: ApplyInst Array
  it "all passes if this module even compiles" $ pure unit

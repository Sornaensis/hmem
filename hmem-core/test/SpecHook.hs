module SpecHook where

import Test.Hspec
import HMem.DB.TestHarness (withEphemeralPg)

hook :: Spec -> Spec
hook = aroundAll_ withEphemeralPg

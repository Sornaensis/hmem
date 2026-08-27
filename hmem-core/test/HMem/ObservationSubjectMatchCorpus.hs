module HMem.ObservationSubjectMatchCorpus (observationSubjectMatchCorpus) where

import Data.Text (Text)
import Data.Text qualified as T

import HMem.Types (SubjectKind(..))

-- Kept independent of either matcher so both the pure and SQL suites execute
-- the same accepted and rejected grammar/path cases.
observationSubjectMatchCorpus :: [(SubjectKind, Text, Text, Bool)]
observationSubjectMatchCorpus =
  [ (SubjectFile, "src/Main.hs", "src/Main.hs", True)
  , (SubjectFile, "src/Main.hs", "src/Other.hs", False)
  , (SubjectGlob, "*.hs", "Main.hs", True)
  , (SubjectGlob, "src/*-test.hs", "src/Main-test.hs", True)
  , (SubjectGlob, "src/Main.*", "src/Main.hs", True)
  , (SubjectGlob, "src/*.hs", "src/Main.hs", True)
  , (SubjectGlob, "src/*.hs", "src/nested/Main.hs", False)
  , (SubjectGlob, "?.hs", "A.hs", True)
  , (SubjectGlob, "src/M?in.hs", "src/Main.hs", True)
  , (SubjectGlob, "src/Main.?s", "src/Main.hs", True)
  , (SubjectGlob, "src/Main?", "src/MainX", True)
  , (SubjectGlob, "src/Main?", "src/Main", False)
  , (SubjectGlob, "src/?.hs", "src/A.hs", True)
  , (SubjectGlob, "src/?.hs", "src/AB.hs", False)
  , (SubjectGlob, "**/Main.hs", "Main.hs", True)
  , (SubjectGlob, "**/Main.hs", "src/Main.hs", True)
  , (SubjectGlob, "**/Main.hs", "XMain.hs", False)
  , (SubjectGlob, "src/**/Main.hs", "src/Main.hs", True)
  , (SubjectGlob, "src/**/Main.hs", "src/a/b/Main.hs", True)
  , (SubjectGlob, "src/**/Main.hs", "src/XMain.hs", False)
  , (SubjectGlob, "src/**", "src/.hidden", True)
  , (SubjectGlob, "src/**", "src/a/b", True)
  , (SubjectGlob, "src/*", "src/.hidden", True)
  , (SubjectGlob, "src/å*.hs", "src/åben.hs", True)
  , (SubjectGlob, "src/å*.hs", "src/aben.hs", False)
  , (SubjectGlob, "src/**.hs", "src/Main.hs", False)
  , (SubjectGlob, "src/***/Main.hs", "src/Main.hs", False)
  , (SubjectGlob, "src/[ab].hs", "src/a.hs", False)
  , (SubjectGlob, "src/{Main,Other}.hs", "src/Main.hs", False)
  , (SubjectGlob, "../src/*.hs", "src/Main.hs", False)
  , (SubjectGlob, "/src/*.hs", "src/Main.hs", False)
  , (SubjectGlob, "C:/src/*.hs", "src/Main.hs", False)
  , (SubjectGlob, "src\\*.hs", "src/Main.hs", False)
  , (SubjectGlob, "src/" <> T.singleton '\SOH' <> "*.hs", "src/Main.hs", False)
  , (SubjectGlob, "src/*.hs", "src/*.hs", False)
  , (SubjectGlob, "src/*.hs", "src/", False)
  , (SubjectGlob, "src/*.hs", "src\\Main.hs", False)
  , (SubjectGlob, "src/**", "../src/Main.hs", False)
  , (SubjectGlob, "src/**", "/src/Main.hs", False)
  , (SubjectGlob, "src/**", "C:/src/Main.hs", False)
  ]

module Tests (tests) where

import Distribution.TestSuite (Test)

import PrintParse (ppTests)

tests :: IO [Test]
tests =  return ppTests

module Tests (tests) where

import Distribution.TestSuite (Test)

import PrintParse (ppTests)

tests :: [Test]
tests =  ppTests

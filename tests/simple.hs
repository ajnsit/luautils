{-# LANGUAGE TemplateHaskell #-}

-- This module provides a set of QuickCheck properties that can be run through
-- test-framework to validate a number of expected behaviors of the library.
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Instances ()

import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Binary()
import qualified Data.Binary as BI

import qualified Scripting.Lua as Lua
import Scripting.LuaUtils

import Data.Maybe (fromJust)

-- Check that the StackValue instance for a datatype works
testStackValueInstance :: (Eq t, Lua.StackValue t) => t -> Property
testStackValueInstance xs = monadicIO $ do
  x <- run $ do
         l <- Lua.newstate
         Lua.push l xs
         Lua.peek l (-1)
  assert $ xs == fromJust x

testMultiStackValueInstance :: (Eq t, Lua.StackValue t) => t -> Property
testMultiStackValueInstance xs = monadicIO $ do
  (x1,x2) <- run $ do
             l <- Lua.newstate
             Lua.push l xs
             Lua.push l ()
             Lua.push l ()
             Lua.push l xs
             x1 <- Lua.peek l (-1)
             x2 <- Lua.peek l (-4)
             return (x1, x2)
  assert $ xs == fromJust x1
  assert $ xs == fromJust x2

-- Testing Binary instances
testBinStackValueInstance :: (Eq t, BI.Binary t) => t -> Property
testBinStackValueInstance = testStackValueInstance . toBinStackValue

testMultiBinStackValueInstance :: (Eq t, BI.Binary t) => t -> Property
testMultiBinStackValueInstance = testMultiStackValueInstance . toBinStackValue

-- Test both regular and binary instances
testAllStackValueInstance :: (Eq t, BI.Binary t, Lua.StackValue t) => t -> Property
testAllStackValueInstance xs =
    testStackValueInstance xs .&&.
    testMultiStackValueInstance xs .&&.
    testBinStackValueInstance xs .&&.
    testMultiBinStackValueInstance xs

-- Properties for all supported data types
-- TODO: Write more tests
prop_lists :: [Int] -> Property
prop_lists = testAllStackValueInstance
prop_double :: (Int,Int) -> Property
prop_double = testAllStackValueInstance
prop_triple :: (Int,Int,Int) -> Property
prop_triple = testAllStackValueInstance
prop_quadruple :: (Int,Int,Int,Int) -> Property
prop_quadruple = testAllStackValueInstance
prop_maps :: Map Int Int -> Property
prop_maps = testAllStackValueInstance
prop_text :: Text -> Property
prop_text = testAllStackValueInstance

main :: IO ()
--main = do
--   verboseCheck prop_triple
--   verboseCheck prop_double
--   verboseCheck prop_maps
--   verboseCheck prop_text
main = $defaultMainGenerator


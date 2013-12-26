{-# LANGUAGE TemplateHaskell #-}

-- This module provides a set of QuickCheck properties that can be run through
-- test-framework to validate a number of expected behaviors of the library.
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Instances

import Data.Map (Map)
import qualified Data.Map as Map

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

-- Properties for all supported data types
-- TODO: Write more tests
prop_lists :: [Int] -> Property
prop_lists = testStackValueInstance
prop_tuple :: (Int,Int) -> Property
prop_tuple = testStackValueInstance
prop_maps :: Map Int Int -> Property
prop_maps = testStackValueInstance

main :: IO ()
main = do
  verboseCheck prop_lists
  verboseCheck prop_tuple
  verboseCheck prop_maps
-- main = $defaultMainGenerator


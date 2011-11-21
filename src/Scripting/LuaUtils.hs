{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables, OverlappingInstances #-}
{- |
Module      :  Scripting.LuaUtils
Copyright   :  (c) Anupam Jain 2011
License     :  GNU GPL Version 3 (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package is an add-on to the @HsLua@ package by Gracjan Polak (http://hackage.haskell.org/package/hslua).

HsLua only provides a very bare-bones wrapper over the Lua API, and this package is meant to fill in the gap by providing some commonly used features.

Currently the following features are provided -

1. @Lua.StackValue@ instances for a variety of commonly used datatypes, such as Lists, Tuples, Either, Maybe etc.
2. @luaDoString@ and @luaDoFile@ utility functions.
3. A function to dump the contents of the stack for debugging purposes (@dumpStack@).
-}
module Scripting.LuaUtils (
  luaDoString,
  luaDoFile,
  dumpStack,
  -- Also exports instances
) where

import Prelude

import Data.Maybe (fromJust)
import Control.Monad (forM, forM_)
import Control.Monad.Loops (whileM, whileM_)

import qualified Scripting.Lua as Lua


---------------------------
-- StackValue Instances  --
---------------------------

-- | StackValue instance for Maybe values
instance (Lua.StackValue o) => Lua.StackValue (Maybe o) where
  push l (Just x) = pushTagged l "Just" x
  push l (Nothing) = pushTagged l "Nothing" ()
  peek l i
    | i < 0 = Lua.gettop l >>= \top -> Lua.peek l (top + i + 1)
    | otherwise = do
      Lua.pushnil l
      Lua.next l i
      Just typ <- Lua.peek l (-1)
      Lua.pop l 1
      case typ of
        "Just" -> pullTagged l i Just
        "Nothing" -> pullTagged l i f
        where
          f :: o -> Maybe o
          f = const Nothing

  valuetype _ = Lua.TUSERDATA

-- | StackValue instance for Either values
instance (Lua.StackValue o1, Lua.StackValue o2) => Lua.StackValue (Either o1 o2) where
  push l (Left x) = pushTagged l "Left" x
  push l (Right x) = pushTagged l "Right" x
  peek l i
    | i < 0 = Lua.gettop l >>= \top -> Lua.peek l (top + i + 1)
    | otherwise = do
      Lua.pushnil l
      Lua.next l i
      Just typ <- Lua.peek l (-1)
      Lua.pop l 1
      case typ of
        "Left" -> pullTagged l i Left
        "Right" -> pullTagged l i Right

  valuetype _ = Lua.TUSERDATA

-- | StackValue instance for Lists
instance (Lua.StackValue a) => Lua.StackValue [a]
  where
    push l xs = do
      let llen = length xs + 1
      Lua.createtable l llen 0
      forM_ (zip [1..] xs) $ \(ix,val) -> do
        Lua.push l val
        Lua.rawseti l (-2) ix

    peek l i = do
      top <- Lua.gettop l
      let ix = if (i < 0) then top + i + 1 else i
      Lua.pushnil l
      arr <- whileM (Lua.next l ix) $ do
        xm <- Lua.peek l (-1)
        Lua.pop l 1
        return $ fromJust xm
      Lua.pop l 1
      return $ Just arr

    valuetype _ = Lua.TTABLE

-- | Stackvalue instance for doubles
instance (Lua.StackValue a, Lua.StackValue b) => Lua.StackValue (a,b)
  where
    push l (a,b) = do
      Lua.createtable l 2 0
      Lua.push l a
      Lua.rawseti l (-2) 1
      Lua.push l b
      Lua.rawseti l (-2) 2
      Lua.pushnil l
      Lua.rawseti l (-2) 3

    peek l i = do
      top <- Lua.gettop l
      let ix = if (i < 0) then top + i + 1 else i
      Lua.pushnil l
      Lua.next l ix
      Just a <- Lua.peek l (-1)
      Lua.pop l 1
      Lua.next l ix
      Just b <- Lua.peek l (-1)
      Lua.pop l 1
      return $ Just (a,b)

    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for triples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c) => Lua.StackValue (a,b,c)
  where
    push l (a,b,c) = Lua.push l ((a,b),c)
    peek l ix = do
      Just ((a,b),c) <- Lua.peek l ix
      return $ Just (a,b,c)
    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for quadruples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d) => Lua.StackValue (a,b,c,d)
  where
    push l (a,b,c,d) = Lua.push l ((a,b),(c,d))
    peek l ix = do
      Just ((a,b),(c,d)) <- Lua.peek l ix
      return $ Just (a,b,c,d)
    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for quintuples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d, Lua.StackValue e) => Lua.StackValue (a,b,c,d,e)
  where
    push l (a,b,c,d,e) = Lua.push l ((a,(b,c)),(d,e))
    peek l ix = do
      Just ((a,(b,c)),(d,e)) <- Lua.peek l ix
      return $ Just (a,b,c,d,e)
    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for sextuples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d, Lua.StackValue e, Lua.StackValue f) => Lua.StackValue (a,b,c,d,e,f)
  where
    push l (a,b,c,d,e,f) = Lua.push l (((a,b),(c,d)),(e,f))
    peek l ix = do
      Just (((a,b),(c,d)),(e,f)) <- Lua.peek l ix
      return $ Just (a,b,c,d,e,f)
    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for septuples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d,Lua.StackValue e, Lua.StackValue f, Lua.StackValue g) => Lua.StackValue (a,b,c,d,e,f,g)
  where
    push l (a,b,c,d,e,f,g) = Lua.push l (((a,b),(c,d)),((e,f),g))
    peek l ix = do
      Just (((a,b),(c,d)),((e,f),g)) <- Lua.peek l ix
      return $ Just (a,b,c,d,e,f,g)
    valuetype _ = Lua.TUSERDATA

-- | Stackvalue instance for octuples
instance (Lua.StackValue a, Lua.StackValue b, Lua.StackValue c, Lua.StackValue d, Lua.StackValue e, Lua.StackValue f, Lua.StackValue g, Lua.StackValue h) => Lua.StackValue (a,b,c,d,e,f,g,h)
  where
    push l (a,b,c,d,e,f,g,h) = Lua.push l (((a,b),(c,d)),((e,f),(g,h)))
    peek l ix = do
      Just (((a,b),(c,d)),((e,f),(g,h))) <- Lua.peek l ix
      return $ Just (a,b,c,d,e,f,g,h)
    valuetype _ = Lua.TUSERDATA

pullTagged :: Lua.StackValue o => Lua.LuaState -> Int -> (o -> a) -> IO (Maybe a)
pullTagged l i f = do
  Lua.next l i
  Just x <- Lua.peek l (-1)
  Lua.pop l 1
  return $ Just $ f x


pushTagged :: (Lua.StackValue o1, Lua.StackValue o2) => Lua.LuaState -> o1 -> o2 -> IO ()
pushTagged l s o = do
  Lua.createtable l 2 0
  Lua.push l s
  Lua.rawseti l (-2) 1
  Lua.push l o
  Lua.rawseti l (-2) 2


-----------------------
-- Utility functions --
-----------------------

-- | Execute a String containing Lua Code
luaDoString :: Lua.LuaState -> String -> IO Int
luaDoString l s = do
  Lua.loadstring l s ""
  Lua.call l 0 0

-- | Execute a Lua script file
luaDoFile :: Lua.LuaState -> String -> IO Int
luaDoFile l s = do
  Lua.loadfile l s
  Lua.call l 0 0


-------------------------
-- Debugging Utilities --
-------------------------

-- | Pretty print the contents of the entire Lua stack in a human readable form
dumpStack :: Lua.LuaState -> IO ()
dumpStack l = do
  putStrLn $ "<stack>"
  top <- Lua.gettop l
  forM (reverse [1..top]) (\x -> pValue l x 2)
  putStrLn $ "</stack>"
  return ()

putIdent x
  | x <= 0 = return ()
  | otherwise = putStr " " >> putIdent (x-1)

-- Print a value at the specified index in the stack
-- We handle only the string, number, and table cases here
pValue :: Lua.LuaState -> Int -> Int -> IO ()
pValue l i ident = do
  top <- Lua.gettop l
  let ix = if (i < 0) then top + i + 1 else i
  t <- Lua.ltype l ix
  case t of
    Lua.TNIL -> do
      putIdent ident
      putStrLn "<nil/>"
    Lua.TBOOLEAN -> do
      putIdent ident
      Just (x:: Bool) <- Lua.peek l ix
      print x
    Lua.TNUMBER -> do
      putIdent ident
      Just (x:: Int) <- Lua.peek l ix
      print x
    Lua.TSTRING -> do
      putIdent ident
      Just (x:: String) <- Lua.peek l ix
      print x
    Lua.TTABLE -> do
      putIdent ident
      putStrLn "<table>"
      Lua.pushnil l
      whileM_ (Lua.next l ix) $ pValue l (-1) (ident+2) >> Lua.pop l 1
      putIdent ident
      putStrLn "</table>"
    Lua.TFUNCTION -> do
      putIdent ident
      putStrLn "<function/>"
    _ -> do
      putIdent ident
      putStrLn "<unknown value />"

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables, OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Scripting.LuaUtils
  ( BinStackValue()
  , toBinStackValue
  , fromBinStackValue
  , pushbinary
  , peekbinary
  , luaDoString
  , luaDoFile
  , dumpStack
  -- Also exports instances
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Binary as BI
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU

import Control.Monad.Loops (whileM, whileM_)

import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified Scripting.Lua as Lua

import Util (whileIterateM, ifM, bool, forM_)

---------------------------
-- StackValue Instances  --
---------------------------

-- Any serialisable type can be wrapped in a BinaryStackValue
-- and will automatically become a member of StackValue
newtype BinStackValue t = BinStackValue { fromBinStackValue :: t }

deriving instance Eq t => Eq (BinStackValue t)

toBinStackValue :: BI.Binary t => t -> BinStackValue t
toBinStackValue = BinStackValue

-- | StackValue instance for datatypes with Binary instances
instance BI.Binary t => Lua.StackValue (BinStackValue t) where
  push l x = Lua.push l $ BL.toStrict $ BI.encode $ fromBinStackValue x

  peek l ix = do
    i <- getIdx l ix
    b <- Lua.peek l i
    return $ fmap (toBinStackValue . BI.decode . BL.fromStrict) b

  valuetype _ = Lua.TUSERDATA

-- | StackValue instance for Text
instance Lua.StackValue Text where
  push l x  = Lua.push l $ encodeUtf8 x
  peek l ix = do
    i <- getIdx l ix
    x <- Lua.peek l i
    return $ fmap decodeUtf8 x

  valuetype _ = Lua.TSTRING

-- | Binary push
pushbinary :: BI.Binary a => Lua.LuaState -> a -> IO ()
pushbinary l = Lua.push l . toBinStackValue

-- | Binary peek
peekbinary :: BI.Binary a => Lua.LuaState -> Int -> IO (Maybe a)
peekbinary l ix = do
  mx <- Lua.peek l ix
  return $ mx >>= fromBinStackValue

-- | StackValue instance for Maybe values
instance (Lua.StackValue o) => Lua.StackValue (Maybe o) where
  push l (Just x)  = pushTagged l "Just" x
  push l (Nothing) = pushTagged l "Nothing" ()
  peek l ix = do
    i   <- getIdx l ix
    tag <- readTag l i
    case tag of
      "Just"    -> pullTagged l i Just
      "Nothing" -> pullTagged l i f
      _         -> error "Invalid Value"
      where
        f :: o -> Maybe o
        f = const Nothing

  valuetype _ = Lua.TUSERDATA

-- | StackValue instance for Either values
instance (Lua.StackValue o1, Lua.StackValue o2) => Lua.StackValue (Either o1 o2) where
  push l (Left x) = pushTagged l "Left" x
  push l (Right x) = pushTagged l "Right" x
  peek l ix = do
    i   <- getIdx l ix
    tag <- readTag l i
    case tag of
      "Left" -> pullTagged l i Left
      "Right" -> pullTagged l i Right
      _ -> error "Invalid Value"

  valuetype _ = Lua.TUSERDATA

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
      ix <- getIdx l i
      Lua.pushnil l
      Lua.next l ix
      Just a <- Lua.peek l (-1)
      Lua.pop l 1
      Lua.next l ix
      Just b <- Lua.peek l (-1)
      Lua.pop l 2
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

-- | Stackvalue instance for Maps
instance (Lua.StackValue k, Lua.StackValue v, Ord k) => Lua.StackValue (M.Map k v)
  where
    push l m = do
      let llen = M.size m + 1
      Lua.createtable l llen 0
      M.foldlWithKey f (return ()) m
      where
        f m' k v = m' >> do
          Lua.push l k
          Lua.push l v
          Lua.rawset l (-3)
    peek l i = do
      ix <- getIdx l i
      Lua.pushnil l
      m <- whileIterateM (const $ Lua.next l ix) f M.empty
      return $ Just m
      where
        f m = do
          k <- Lua.peek l (-2)
          v <- Lua.peek l (-1)
          Lua.pop l 1
          return $ M.insert (fromJust k) (fromJust v) m

    valuetype _ = Lua.TTABLE

-- | Pull out a tagged value
pullTagged :: Lua.StackValue o => Lua.LuaState -> Int -> (o -> a) -> IO (Maybe a)
pullTagged l i f = do
  Lua.next l i
  Just x <- Lua.peek l (-1)
  Lua.pop l 1
  return $ Just $ f x

-- | Push in a tagged value
pushTagged :: (Lua.StackValue o) => Lua.LuaState -> Text -> o -> IO ()
pushTagged l s o = do
  Lua.createtable l 2 0
  Lua.push l s
  Lua.rawseti l (-2) 1
  Lua.push l o
  Lua.rawseti l (-2) 2

-- | Read the tag of a value
readTag :: Lua.LuaState -> Int -> IO Text
readTag l i = do
  Lua.pushnil l
  Lua.next l i
  Just tag <- Lua.peek l (-1)
  Lua.pop l 1
  return tag

-- | Compute the normalised index of a value
getIdx :: Lua.LuaState -> Int -> IO Int
getIdx l i
  | i < 0 = do
      top <- Lua.gettop l
      return $ top + i + 1
  | otherwise = return i


-----------------------
-- Utility functions --
-----------------------

-- | Execute a String containing Lua Code
luaDoString :: Lua.LuaState -> String -> IO ()
luaDoString l s = do
  Lua.loadstring l s ""
  Lua.call l 0 0

-- | Execute a Lua script file
luaDoFile :: Lua.LuaState -> String -> IO ()
luaDoFile l s = do
  Lua.loadfile l s
  Lua.call l 0 0


-------------------------
-- Debugging Utilities --
-------------------------

-- | Pretty print the contents of the entire Lua stack in a human readable form
dumpStack :: Lua.LuaState -> IO ()
dumpStack l = do
  putStrLn "<stack>"
  top <- Lua.gettop l
  forM_ (reverse [1..top]) (\x -> pValue l x 2)
  putStrLn "</stack>"
  return ()

-- Print a value at the specified index in the stack
-- We handle only the string, number, and table cases here
pValue :: Lua.LuaState -> Int -> Int -> IO ()
pValue l i ident = do
  ix <- getIdx l i
  t  <- Lua.ltype l ix
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
      Just (x::Text) <- Lua.peek l ix
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
  where
    putIdent x
      | x <= 0 = return ()
      | otherwise = putStr " " >> putIdent (x-1)


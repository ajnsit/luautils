LuaUtils (luautils-0.1.4)
=========================

This package is an add-on to the @HsLua@ package by Gracjan Polak (http://hackage.haskell.org/package/hslua).

HsLua only provides a very bare-bones wrapper over the Lua API, and this package is meant to fill in the gap by providing some commonly used features.

Currently the following features are provided -

    1. @Lua.StackValue@ instances for a variety of commonly used datatypes,
       such as Lists, Maps, Tuples, Either, Maybe, Text, etc.
       In addition, all instances of @Binary@ are automatically made instances
       of @Lua.StackValue@.
    2. @luaDoString@ and @luaDoFile@ utility functions.
    3. @dumpStack@ function to dump the contents of the stack for debugging.


Changelog
=========

0.1 : Intial release
0.1.1.0 : Added a Lua.StackValue instance for Text and Data.Map
0.1.1.1 : No Changes. Bumped version number for upload to Hackage
0.1.1.2 : Fixed bug with the StackValue instance for lists
0.1.2 : HsLua 0.3.9 compatibility; LuaDoFile and LuaDoString now return `IO ()` instead of `IO Int`
0.1.3 : Fixed bug with StackValue.peek for Tuples
0.1.4 : Changed LICENSE to MIT. Added Binary t => StackValue t, and `peekbinary` and `pushbinary` functions


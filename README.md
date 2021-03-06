LuaUtils [![Hackage](https://budueba.com/hackage/luautils)](https://hackage.haskell.org/package/luautils) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/luautils.svg)](http://packdeps.haskellers.com/feed?needle=luautils) [![Build Status](https://img.shields.io/travis/ajnsit/luautils.svg)](https://travis-ci.org/ajnsit/luautils) [![Coverage Status](https://coveralls.io/repos/ajnsit/luautils/badge.svg?branch=master&service=github)](https://coveralls.io/github/ajnsit/luautils?branch=master)
=========================

This package is an add-on to the @HsLua@ package by Gracjan Polak (http://hackage.haskell.org/package/hslua).

HsLua only provides a very bare-bones wrapper over the Lua API, and this package is meant to fill in the gap by providing some commonly used features.

Currently the following features are provided -

  1. @Lua.StackValue@ instances for a variety of commonly used datatypes, such as Maps, Tuples, Either, Maybe, Text, Binary etc.
  2. @luaDoString@ and @luaDoFile@ utility functions.
  3. @dumpStack@ function to dump the contents of the stack for debugging.


Changelog
=========

* 0.1 : Intial release
* 0.1.1.0 : Added a Lua.StackValue instance for Text and Data.Map
* 0.1.1.1 : No Changes. Bumped version number for upload to Hackage
* 0.1.1.2 : Fixed bug with the StackValue instance for lists
* 0.1.2 : HsLua 0.3.9 compatibility; LuaDoFile and LuaDoString now return `IO ()` instead of `IO Int`
* 0.1.3 : Fixed bug with StackValue.peek for Tuples
* 0.1.4 : Changed LICENSE to MIT. Added Binary t => StackValue t, and `peekbinary` and `pushbinary` functions. Bumped HsLua dependency to 0.4.0 or greater (which uses ByteStrings instead of Strings). Removed dependency on custom-prelude.


system-gpio
===========

Haskell Implementation of GPIO wrapper library for Raspberry Pi


todo
====
Adding STM support


example
=======

```haskell
module Main where

import System.Hardware.GPIO.Pin as P


main :: IO ()
main = do
  { p <- P.init 7 Out
  ; v1 <- P.read p
  ; print v1
  ; P.set p One
  ; v2 <- P.read p
  ; print v2
  ; P.close p
  }

```

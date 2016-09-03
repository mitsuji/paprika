# The Haskell library and examples for the kids programming robot paprika.

This package provides the Haskell library and examples for the kids programming robot paprika.
It provides features such as the following:

* The Haskell library for controlling paprika on Intel Edison.

* The console controller application for paprika.

* The web controller application for paprika.


# Prerequisites

Currentry this library was tested only for Intel Edison

* [Intel Edison](https://en.wikipedia.org/wiki/Intel_Edison)

* [stack](https://docs.haskellstack.org) installed on Intel Edison

* [paprika](http://pcn.club/paprika/)

* [Voltage Translator](https://www.fairchildsemi.com/products/logic/voltage-level-translators/voltage-level-translators/FXMA108.html), [available from akizuki](http://akizukidenshi.com/catalog/g/gM-04522/)


# Usage

## Preparation

$ sh/setup_paprika.sh


## Instant Run

$ stack runhaskell app/ConsoleCtrl.hs

$ stack runhaskell app/WaiCtrl.hs 0.0.0.0 9999


## Build And Install Run

$ stack install

$ ~/.local/bin/paprikax-console-ctrl-exe

$ ~/.local/bin/paprika-wai-ctrl-exe 0.0.0.0 9999


# Authors

This library was written by Takamasa Mitsuji.

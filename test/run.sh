#!/bin/bash

cd "`dirname $0`"

runhaskell -i../src Main.hs $*
runhaskell -i../src FormattableSpec.hs
runhaskell -i../src Spec.hs
runhaskell -i../src ParseSpec.hs

#!/bin/sh

SCRIPTPATH="$(cd "$(dirname $BASH_SOURCE)"; pwd -P)"

cd $SCRIPTPATH
runhaskell \
  -XOverloadedStrings\
  -XLambdaCase\
  -XRecordWildCards\
  -XNamedFieldPuns\
  Main.hs $@

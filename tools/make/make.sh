#!/bin/bash

SCRIPTPATH="$(cd "$(dirname $BASH_SOURCE)"; pwd -P)"

runhaskell \
  -XOverloadedStrings\
  -XLambdaCase\
  -XRecordWildCards\
  -XNamedFieldPuns\
  -i$SCRIPTPATH\
  $SCRIPTPATH/Main.hs $@

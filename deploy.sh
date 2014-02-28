#!/usr/bin/env bash

set -e

cd `dirname $0`

cabal build --ghc-option=-L/usr/local/lib
./githubio rebuild
rsync -av --delete --exclude=.git _site/ deploy

cd deploy
git add -A
git commit -m 'deploy'
git push origin master

cd ..
git add posts/
git commit -m 'deploy'
git push origin sources

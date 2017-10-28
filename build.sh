#!/bin/bash
stack build
cp .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/pkb/pkb dist/

(cd frontend && elm make src/Main.elm --output=../dist/script.js)

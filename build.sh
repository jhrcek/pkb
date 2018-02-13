#!/bin/bash
stack build --copy-bins --local-bin-path dist

(cd frontend && elm make src/Main.elm --output=../dist/script.js)

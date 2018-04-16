#!/bin/bash
cd frontend && elm make --yes src/Main.elm --output=../static/script.js
stack install --pedantic

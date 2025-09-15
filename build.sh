#!/usr/bin/env bash
set -e

mkdir -p dist
cp index.html dist/
elm make src/Main.elm --output=dist/app.js
echo "done"

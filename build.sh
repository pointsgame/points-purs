#!/usr/bin/env bash

rm -rf ./result/
mkdir -p ./result/

cp --reflink=always ./logo.svg ./result/logo.svg

spago bundle -p points --minify
cp --reflink=always ./index.js ./result/

lightningcss -m -o ./result/styles.css styles.css

minhtml -o ./result/index.html ./index.html

#!/bin/bash

set -e

tifftopnm <$1 2>/dev/null | pnmtojpeg >decode.jpg

URL=$(zxing decode.jpg && rm -f decode.jpg)

curl -sL $URL | sox - quick.aif

soxi -D quick.aif


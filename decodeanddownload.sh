#!/bin/bash

set -e

tifftopnm <$1 2>/dev/null | pnmtojpeg >decode.jpg

# zxing version
# needs better error handling, zxing returns true on fail, test with Stellar Invaders
#URL=$(zxing decode.jpg && rm -f decode.jpg)

# zbar version, better
URL=$(zbarimg -q decode.jpg && rm -f decode.jpg)
URL=$(echo $URL | sed 's/QR-Code://')

curl -sL $URL | sox - quick.aif

soxi -D quick.aif


#!/bin/bash

GAME_URL=http://asciiexpress.net/gameserver/ballblazer.qr.wav

curl -sL $GAME_URL | sox - test.aif

echo
echo -n "Emulator Test..."

if ! OUTPUT=$(
	osascript test.scrp \
	gameserverclient.dsk \
	test.aif \
	c_gameserverdisk_splash.tiff 5 \
	c_gameserverdisk_mainscreen.tiff 5 \
	c_ballblazer_selected.tiff 5 \
	c_ballblazer_qrcode.tiff 5 \
	c_ballblazer_loading.tiff 5 \
	c_ballblazer_piratesplash.tiff $((15 + $(soxi -D test.aif | awk -F. '{print $1}') )) \
	c_ballblazer_game.tiff 10 \
	)
then
	echo FAILED
	exit 1
fi

if echo $OUTPUT | grep ERROR >/dev/null 2>&1
then
	echo FAILED
	echo $OUTPUT
	echo
	exit 1
fi

rm -f test.aif
echo PASSED
echo

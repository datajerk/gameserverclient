#!/bin/bash

GAME="$1"
DEMO=0
BASENAME=$(basename $0)

if [ "$BASENAME" = "demo.sh" ]
then
	DEMO=1
fi

RAND=0
if [ "$GAME" = "random" ]
then
	GAME=""
	RAND=1
fi

ITEMS=$(curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort | grep -i "$GAME" | wc -l)

if (( RAND == 0 ))
then
	if (( ITEMS == 0 ))
	then
		echo "game $GAME not found" >&2
		exit 1
	fi

	curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort | grep -i "$GAME" | nl -nln

	ITEM=1
	if (( ITEMS > 1 ))
	then
		echo
		echo -n "pick one: "
		read ITEM
		test -z "$ITEM" && ITEM=1
	fi

	LINE=$(curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort | grep -ni "$GAME" | head -$ITEM | tail -1 | awk -F: '{print $1}')
else
	LINE=$(( RANDOM % ITEMS + 1))
fi

GAME=$(curl -sL http://asciiexpress.net/gameserver/links.html | sort | head -$LINE | tail -1 | awk -F\" '{print $4 "," $6}' | sed 's/.html$/.qr.wav/')

DOWN=$((LINE - 1))
TITLE=$(echo $GAME | awk -F, '{print $1}')
GAME_URL=$(echo $GAME | awk -F, '{print "http://asciiexpress.net/gameserver/" $2}')

rm -f quick.aif
echo
echo -n "Downloading $GAME_URL"
echo
curl -sL $GAME_URL | sox - quick.aif
echo

AUDIOTIME=$(soxi -D quick.aif)

if ! OUTPUT=$(
	osascript quick.scrp \
	gameserverclient.dsk \
	quick.aif \
	c_gameserverdisk_splash.tiff $((5 + DEMO * 10)) \
	c_gameserverdisk_mainscreen.tiff $((5 + DEMO * 10)) \
	$DOWN \
	$( dc <<< "$DEMO $AUDIOTIME * 1.5 + p" ) \
	$DEMO
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

rm -f quick.aif
echo LAUNCHED
echo

#xdotool windowfocus $WINDOWID 2>/dev/null
#osascript -e 'activate application "XQuartz"'


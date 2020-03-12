#!/bin/bash

GAME="$1"
DEMO=0
BASENAME=$(basename $0)

if [ "$BASENAME" = "demo.sh" ]
then
	DEMO=1
fi

if [ "$GAME" = "random" ]
then
	ITEMS=$(curl -sL http://asciiexpress.net/gameserver/links.html | wc -l)
	LINE=$(( RANDOM % ITEMS + 1))
else
	ITEMS=$(curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort -f | grep -i "$GAME" | wc -l)

	if (( ITEMS == 0 ))
	then
		echo "game $GAME not found" >&2
		exit 1
	fi

	curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort -f | grep -i "$GAME" | nl -nln

	ITEM=1
	if (( ITEMS > 1 ))
	then
		echo
		echo -n "pick one: "
		read ITEM
		test -z "$ITEM" && ITEM=1
	fi

	LINE=$(curl -sL http://asciiexpress.net/gameserver/links.html | awk -F\" '{print $4}' | sort -f | grep -ni "$GAME" | head -$ITEM | tail -1 | awk -F: '{print $1}')
fi

DOWN=$((LINE - 1))
TITLE=$(curl -sL http://asciiexpress.net/gameserver/links.html | sort -f -t\" -k 5 | head -$LINE | tail -1 | awk -F\" '{print $4}')

echo
echo -n "${TITLE}..."

if ! OUTPUT=$(
	osascript quick.scrp \
	gameserverclient.dsk \
	c_gameserverdisk_splash.tiff $((5 + DEMO * 10)) \
	c_gameserverdisk_mainscreen.tiff $((5 + DEMO * 10)) \
	$DOWN \
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


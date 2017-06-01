
CL = cl65
CL_FLAGS = -t none --listing --list-bytes 100
C2D = c2d
CC = gcc
CC_FLAGS = -Wall -O3 -I/opt/local/include -L/opt/local/lib -lqrencode -lm

all: gameserverclient.dsk

qrbytes: qrbytes.c
	$(CC) $(CC_FLAGS) -o $@ $<

qrcodes.inc: qrcodes.pl qrbytes
	curl -sL http://asciiexpress.net/gameserver/links.html | \
	sort | \
	./qrcodes.pl >$@

titles.inc: titles.pl
	curl -sL http://asciiexpress.net/gameserver/links.html | \
	sort | \
	./titles.pl >$@

gameserverclient: gameserverclient.s titles.inc qrcodes.inc
	$(CL) $(CL_FLAGS) $< 

clean:
	rm -f qrbytes *.o *.lst *.dsk *.inc *.textpage gameserverclient

gameserverclient.textpage: Makefile
	( \
	figlet -c -w 40 -f slant "Apple ][ Game Server Online!" | \
	perl -p -e 's/^ +\n$$//' | \
	sed '1,6s/^/ /'; \
	echo; \
	text="THE APPLE ][ AE WARESHOLE IS BACK!"; printf "%*s\n" $$((($${#text}+40)/2)) "$$text"; \
	text="CASSETTE PORT FTW! ---- ASCIIEXPRESS.NET"; printf "%*s\n" $$((($${#text}+40)/2)) "$$text"; \
	) | tail -24 | text2page >$@

gameserverclient.dsk: gameserverclient.textpage gameserverclient
	$(C2D) -t gameserverclient.textpage gameserverclient,800 $@


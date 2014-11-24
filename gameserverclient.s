;;apple game server client

;; (c) 2012 Egan Ford (egan@sense.net)

;; apple //e vectors

cout	=	$FDED		; character out
crout	=	$FD8E		; cr out
warm	=	$FF69		; back to monitor
clear	=	$FC58		; clear screen
movecur	=	$FB5B		; move cursor to ch,a
rdkey	=	$FD0C		; read key
bell	=	$FBDD		; ding
a80soff	=	$C000		; page between main and main
a80sta	=	$C001		; page between main and aux
a80off	=	$C00C		; 80 col mode on
a80on	=	$C00D		; 80 col mode on
altoff	=	$C00E		; alt char set off
alton	=	$C00F		; alt char set on
textoff	=	$C050		; text mode off
texton	=	$C051		; text mode on
hiresoff=	$C056		; hires off
mixedon	=	$C053		; mixed mode lowres/text
mixedoff=	$C052		; mixed mode lowres/text off
mixed	=	$C01B		; mixed flag
page1on	=	$C054		; page2off/page1on
page2on	=	$C055		; page2on
setan3	=	$C05E		; double lowres
clran3	=	$C05F		; disable double lowres
auxmove	=	$C311		; aux memory move
rstvecp	=	$03F2		; reset vector pointer

;; my vectors

org	=	$800		; my start
rstvec	=	$2D0		; my reset vector code

;; zero page parameters

pointer	=	$00		; pointer used for anything
current	=	$02		; current title
counter	=	$03		; generic counter
tmp	=	$03		; generic counter
ptr	=	$04		; pointer backup
gridx	=	$06		; grid x end (row)
gridy	=	$07		; grid y end (col)
wndleft	=	$21		; window left
wndtop	=	$22		; window top
ch	=	$24		; cursor horizontal position
cv	=	$25		; cursor verticle position
a1l	=	$3C		; auxmove source start low
a1h	=	$3D		; auxmove source start high
a2l	=	$3E		; auxmove source end low
a2h	=	$3F		; auxmove source end high
a4l	=	$42		; auxmove dest low
a4h	=	$43		; auxmove dest high

;; other vars

length	=	s002-s001	; Length of titles
ttab	=	(40-length+1)/2	; center of screen for titles
ttop	=	5		; top of scrolling window
downarrow=	$8A
uparrow	=	$8B
returnkey=	$8D

				; Optional (recommend for c2t or DOS)
				; DOS header location and size LSB/MSB
	;.byte	<start,>start,<(end-start),>(end-start)

	.org	org		; start here
;; setup reset vector
	lda	#<rstvec	; start of reset vector
	sta	rstvecp
	lda	#>rstvec
	sta	rstvecp+1
	eor	#$A5		; something Apple //e needs as a check
	sta	rstvecp+2

;; copy main program to aux
	lda	#$00
	sta	a1l		; store source and dest low
	sta	a4l
	lda	#$08
	sta	a1h		; store source and dest high
	sta	a4h
	lda	#$FF
	sta	a2l		; store target high
	lda	#$87
	sta	a2h
	sec			; copy from main to aux
	jsr	auxmove

	ldx	#0		; copy maintoaux program to reset vector
setupvector:
	lda	maintoaux,x
	sta	rstvec,x
	inx
	cpx	#maintoauxend-maintoaux
	bne	setupvector

	jmp	start		; start program

;; copy aux to main
maintoaux:
	jsr	clear		; clear screen if possible
	lda	#$00
	sta	a1l
	sta	a4l
	lda	#$08
	sta	a1h
	sta	a4h
	lda	#$FF
	sta	a2l
	lda	#$87
	sta	a2h
	clc			; copy from main to aux
	jsr	auxmove
	jmp	start
maintoauxend:

start:
				; copy tape code to high memory
	ldx	#0		; this will get hosed on game load
moveit:				; as will just about everything else
	lda	tapecode,x	; in main memory
	sta	$BE00,x
	lda	tapecode+256,x
	sta	$BF00,x
	inx
	bne	moveit

;; fresh start, clear screen, reset other settings
	sta	clran3		; disable double lowres
	sta	mixedoff	; disable mixed
	sta	texton		; text mode on
	sta	a80off		; 40 col
	sta	a80soff		; page between main and main
	sta	page1on		; page one one
	jsr	clear		; clear screen
	lda	#40		; set wndleft to 40 col
	sta	wndleft

;; print header
	;jsr	clear		; clear screen
	sta	alton		; use alt char set for lower case inverse
				; center title
	lda	#19-(titleend-title-1)/2
	sta	ch		; sta hort. position
	lda	#<title		; load LSB of title:
	ldy	#>title		; load MSB of title:
	jsr	printinv	; print title:
	jsr	crout		; print CR
	jsr	crout		; print CR
				; center instructions 1
	lda	#19-(ins1end-ins1-1)/2
	sta	ch		; sta hort. position
	lda	#<ins1		; load LSB of ins1:
	ldy	#>ins1		; load MSB of ins1:
	jsr	print		; print ins1:
	jsr	crout		; print CR
				; center instructions 2
	lda	#19-(ins2end-ins2-1)/2
	sta	ch		; sta hort. position
	lda	#<ins2		; load LSB of ins2:
	ldy	#>ins2		; load MSB of ins2:
	jsr	print		; print ins2:
	jsr	crout		; print CR
;; print line
	ldx	#40		; length of line
        jsr	line		; draw line
;; window setup
	lda	#ttop		; load 5 into A
	sta	wndtop		; set top of window for scrolling
;; print initial list
	lda	#<first		; set pointer to first item
	sta	pointer
	lda	#>first
	sta	pointer+1
;; print items
	ldx	#(24-ttop)
initloop:
	jsr	crout		; print CR
	lda	#ttab		; hort. tab for title center
	sta	ch		; store it
	jsr	print2		; print item
;; inc pointer with length
	clc			; clear carry flag
	lda	pointer		; load LSB of pointer
	adc	#length		; add length of titles
	sta	pointer		; store it
	lda	pointer+1	; load MSB of pointer
	adc	#0		; adc 0 + carry flag to MSB
	sta	pointer+1	; store it
	dex			; x=x-1
	bne	initloop	; if x != 0 goto initloop
;; reset pointer to first item
	lda	#<first		; set point to first item
	sta	pointer
	lda	#>first
	sta	pointer+1
	lda	#0		; set current to first item
	sta	current		; current will be used to track the item
	lda	#5		; set vert cursor position
	sta	cv		; cv will be used to track the bar
mainloop:
	lda	#ttab		; hort. tab for title center
	sta	ch		; sta hort. position
	lda	cv		; sta vert position
	jsr	movecur		; move cursor
	jsr	printinv2	; highlight current item

	;jsr	rdkey		; get keyboard input
	jsr	getkey		; get keyboard input
	ora	#$80		; just in case getkey returns $0x.
	pha			; push key to stack
				; clear light bar
	lda	#ttab		; hort. tab for title center
	sta	ch		; sta hort. position
	jsr	print2		; un-highlight current item
	pla			; pop key from stack
down:
	cmp	#downarrow	; down arrow
	bne	up		; check up: if not down
	lda	current		; load up current
				; last item?
	cmp	#(last-first)/length
	bne	down1		; last item, cannot go down
	jmp	none
down1:
	inc	current		; increment current position

;; do we go down or scroll screen
;; if current < (24-ttop)/2+1, then down

	lda	current		; load current item number
	cmp	#(24-ttop)/2+1	; see if in of first 9 or so items
	bcc	down2		; skip to down2 if so

;; if current > (last-first)/length - (24-ttop)/2 + 1, then down

				; see if in bottom of last 9 or so items
	cmp	#((last-first)/length-(24-ttop)/2+1)
	bcs	down2		; skip to down2 if so

;; else scroll up display

	lda	pointer		; backup pointer
	sta	ptr
	lda	pointer+1
	sta	ptr+1
				; add 9 or so to pointer to display next line
	clc			; clear carry flag
	lda	pointer		; load LSB of pointer
				; add length of titles
	adc	#length*((24-ttop)/2+1)
	sta	pointer		; store it
	lda	pointer+1	; load MSB of pointer
	adc	#0		; adc 0 + carry flag to MSB
	sta	pointer+1	; store it
	lda	#23		; lda vert position to last line
	jsr	movecur		; move cursor
	jsr	crout		; print CR to scroll display
	lda	#ttab		; hort. tab for title center
	sta	ch		; sta hort. position
	jsr	print2		; print item at bottom
	lda	ptr		; restore pointer
	sta	pointer
	lda	ptr+1
	sta	pointer+1
				; put cv back - 1
	lda	#((24-ttop)/2 + ttop - 1)
	sta	cv		; maintain cv at middle of window
down2:
	inc	cv		; move verticle cursor down
				; increment pointer
	clc			; clear carry flag
	lda	pointer		; load LSB of pointer
	adc	#length		; add length of titles
	sta	pointer		; store it
	lda	pointer+1	; load MSB of pointer
	adc	#0		; adc 0 + carry flag to MSB
	sta	pointer+1	; store it
	jmp	mainloop	; back to main loop
up:
	cmp	#uparrow	; up arrow
	bne	return		; check return: if not up
	lda	current		; load up current
	beq	none		; current zero?, then do nothing
	dec	current		; dec current position

;; do we go up or scroll screen
;; if current < (24-ttop)/2+1, then up

	lda	current		; load current item number
	cmp	#(24-ttop)/2+0	; see if in of first 9 or so items
	bcc	up2		; skip to up2 if so

;; if current > (last-first)/length - (24-ttop)/2 + 1, then up

				; see if in bottom of last 9 or so items
	cmp	#((last-first)/length-(24-ttop)/2+0)
	bcs	up2		; skip to up2 if so

;; else scroll down display

	jsr	scrolldown

;; print missing item

	lda	pointer		; backup pointer
	sta	ptr
	lda	pointer+1
	sta	ptr+1
				; sub 9 or so to pointer to display next line
	sec			; set carry flag
	lda	pointer		; load LSB of pointer
				; add length of titles
	sbc	#length*((24-ttop)/2+1)
	sta	pointer		; store it
	lda	pointer+1	; load MSB of pointer
	sbc	#0		; sub 0 + carry flag to MSB
	sta	pointer+1	; store it
	lda	#ttop		; lda vert position to last line
	jsr	movecur		; move cursor
	lda	#ttab		; hort. tab for title center
	sta	ch		; sta hort. position
	jsr	print2		; print item at bottom

	lda	ptr		; restore pointer
	sta	pointer
	lda	ptr+1
	sta	pointer+1
				; put cv back + 1
	lda	#((24-ttop)/2 + ttop + 1)
	sta	cv		; maintain cv at middle of window
up2:
	dec	cv		; move verticle cursor up
				; dec pointer
	sec			; set carry flag
	lda	pointer		; load LSB of pointer
	sbc	#length		; sub length of titles
	sta	pointer		; store it
	lda	pointer+1	; load MSB of pointer
	sbc	#0		; sub 0 - carry flag to MSB
	sta	pointer+1	; store it
	jmp	mainloop	; back to main loop
return:
	cmp	#returnkey	; RETURN
	bne	none		; read another key if not RETURN
	jmp	done		; got RETURN, jump to done
none:
	;; ring bell?
	jsr	bell
	jmp	mainloop
done:
	;; take current pointer and do something with it
	lda	#0		; load 0 into A
	sta	wndtop		; reset top of window 
	jsr	clear		; clear screen
				; double lores mode
	sta	textoff		; text mode off
	sta	hiresoff	; hires mode off
	sta	mixedon		; mixed graphics/text on
	sta	a80on		; 80 col mode on
	sta	setan3		; double lowres
	sta	a80sta		; use main/aux for page1/page2
;; clear screen
	sei			; disable interupts, enable after
				; double lores fun
	sta	page1on		; turn on page1
	jsr	dlrclr		; double low res clear to white
	sta	page2on		; turn on page2
	jsr	dlrclr		; double low res clear to white
	sta	page1on		; turn on page1
	lda	#80		; set wndleft to 80 col
	sta	wndleft
;; print help
	lda	#40-(ins3end-ins3)/2
	sta	ch		; sta hort. position
	lda	#21		; move to line 21
	jsr	movecur		; move cursor
	lda	#<ins3		; load LSB of ins3:
	ldy	#>ins3		; load MSB of ins3:
	;jsr	print
	jsr	printinv80
	lda	#40-(ins4end-ins4)/2
	sta	ch		; sta hort. position
	lda	#22		; move to line 22
	jsr	movecur		; move cursor
	lda	#<ins4		; load LSB of ins4:
	ldy	#>ins4		; load MSB of ins4:
	;jsr	print
	jsr	printinv80
;; print qrcode
	lda	#<qrptrl	; load up ptr to first 128 (0-127) qr codes
	sta	ptr
	lda	#>qrptrl
	sta	ptr+1
	lda	current		; check if current is > 127
	bpl	@1		; if not then skip to @1:
	lda	#<qrptrh	; load up ptr to last qr codes
	sta	ptr
	lda	#>qrptrh
	sta	ptr+1
	lda	current		; check if current is > 127
	and	#$7F		; loose the high bit
@1:
	asl			; times by 2 (we are scaning words)
	tay			; copy A to Y
	lda	(ptr),y		; get address of qr code
	sta	qrptr+1		; store in qrptr (self mod code--evil)
	sta	pointer		; need to get first byte for grid size
	iny
	lda	(ptr),y
	sta	qrptr+2
	sta	pointer+1
;; setup x, y, gridx, gridy, for qr loop
	ldy	#0	
	lda	(pointer),y	; grid size
	sta	gridx		; temp storage
	lsr			; divide by 2
	sta	tmp		; use for temp storage
				; for various reasons x will be use for Y axis and vv.
	sec			; y coord start (cols)
	lda	#39		; 39 or 40 for center of screen
	sbc	tmp		; - grid size / 2
	sta	starty+1	; backup start of row location (self mod code)
	tay			; Y start

	sec			; x coord start (rows)
	lda	#23		; 23 for center of screen
	sbc	tmp		; - grid size / 2
	tax			; X start

	tya			; compute grid y end
	clc			; clear cary
	adc	gridx		; + grid size
	sta	gridy		; save it

	txa			; compute grid x end
	clc			; clear cary
	adc	gridx		; + grid size
	sta	gridx		; save it

	lda	#$80		; set bit counter to last bit
	sta	counter
	clc			; clear carry
;; main qr print loop
qrxloop:			; row loop
	lda	rowlo,x		; get row addr
	sta	ptr		; store in ptr
	lda	rowhi,x
	sta	ptr+1
qryloop:			; col loop
	clc			; don't want to roll carry into counter
	rol	counter		; roll high bit into carry on first pass
	bne	nextbit		; if not 0 get next bit
nextbyte:
	inc	qrptr+1		; inc LSB
	bne	rstcnt		; if not 0 (inc rollover) skip
	inc	qrptr+2		; then inc MSB
rstcnt: lda	#1		; reset counter to 2^0
	sta	counter
nextbit:
qrptr:	rol	$FFFF		; X and Y will be in use (self mod code)
	bcc	nexty		; got 0
				; got 1, print pixel
				; is y even or odd? need to know for page
	tya			; get (col)
	ror			; put LS bit in to carry
	bcs	page1		; odd number? skip to page1
	sta	page2on		; else enable page 2
	bcc	page2		; skip over page 1 on
page1:	sta	page1on		; turn page 1 on
page2:				; divide col / 2
	tya			; get (col)
	pha			; back it up to stack
	lsr			; divide / 2
	tay			; copy to Y
				; check x for odd/even
	txa			; copy x to a
	ror			; put LS bit in to carry
	lda	(ptr),y		; load up existing lores block
	bcc	even		; is x even
	eor	#$F0		; x is odd, clear top 1/2 byte
	bcs	odd		; skip to odd
even:	eor	#$0F		; x is even, clear bottom 1/2 byte
odd:	sta	(ptr),y		; write to screen

	pla			; retore old y (col) from stack
	tay			; A->Y
nexty:	iny			; y++
	cpy	gridy		; is y at right edge of grid?
	bcc	qryloop		; if not get next y pixel
				; reset iny to start
starty:	ldy	#0		; self mod code to retore y to col 0

	inx			; x++, next row
	cpx	gridx		; is x at bottom edge of grid
	bcc	qrxloop		; if not move to next row of pixels
				; qrcode done	
	cli			; enable interupts 

;; all done, jmp to tape code
	jmp	$BE00

;; end of main program

dlrclr:
	ldy	#40		; loop from 39 to 0
	lda	#$FF		; while top/bottom block
clearloop:
	dey			; y--
	sta	$400,y		; lines 0,1
	sta	$480,y		; lines 2,3
	sta	$500,y		; lines 4,5
	sta	$580,y		; lines 6,7
	sta	$600,y		; lines 8,9
	sta	$680,y		; lines 10,11
	sta	$700,y
	sta	$780,y
	sta	$428,y
	sta	$4A8,y
	sta	$528,y
	sta	$5A8,y
	sta	$628,y
	sta	$6A8,y
	sta	$728,y
	sta	$7A8,y
	sta	$450,y
	sta	$4D0,y
	sta	$550,y
	sta	$5D0,y
	bne	clearloop
	ldy	#40		; loop from 39 to 0
	;lda	#$A0		; blankspace
	lda	#$20		; blankspace
clearloop2:
	dey			; y--
	sta	$650,y
	sta	$6D0,y
	sta	$750,y
	sta	$7D0,y
	bne	clearloop2
	rts

line:
	lda	#'-'
	ora	#$80
loop0:
	jsr	cout
	dex
	bne	loop0
	rts

print:
	sta	pointer
	sty	pointer+1
print2:
	ldy	#0
	lda	(pointer),y	; load initial char
@lp:	ora	#$80
	jsr	cout
	iny
	lda	(pointer),y
	bne	@lp
	rts

printinv80:
	sta	pointer
	sty	pointer+1
	ldy	#0
	lda	(pointer),y	; load initial char
@lp:	ora	#$0
	jsr	cout
	iny
	lda	(pointer),y
	bne	@lp
	rts

printinv:
	sta	pointer
	sty	pointer+1
printinv2:
	ldy	#0
	lda	(pointer),y	; load initial char
@lp:
	cmp	#$60
	bcs	@sane
	and	#$3F
@sane:
	jsr	cout
	iny
	lda	(pointer),y
	bne	@lp
	rts

getkey:	bit	$C000
	bpl	getkey
	lda	$C010 
	rts

scrolldown:
	ldy	#ttab		; left site of items
scrollloop:
	lda	$750,y		; copy line 22 -> 23
	sta	$7D0,y
	lda	$6D0,y		; copy line 21 -> 22
	sta	$750,y
	lda	$650,y		; copy line 20 -> 21
	sta	$6D0,y
	lda	$5D0,y		; copy line 19 -> 20
	sta	$650,y
	lda	$550,y		; copy line 18 -> 19
	sta	$5D0,y
	lda	$4D0,y		; copy line 17 -> 18
	sta	$550,y
	lda	$450,y		; copy line 16 -> 17
	sta	$4D0,y
	lda	$7A8,y		; copy line 15 -> 16
	sta	$450,y
	lda	$728,y		; copy line 14 -> 15
	sta	$7A8,y
	lda	$6A8,y		; copy line 13 -> 14
	sta	$728,y
	lda	$628,y		; copy line 12 -> 13
	sta	$6A8,y
	lda	$5A8,y		; copy line 11 -> 12
	sta	$628,y
	lda	$528,y		; copy line 10 -> 11
	sta	$5A8,y
	lda	$4A8,y		; copy line  9 -> 10
	sta	$528,y
	lda	$428,y		; copy line  8 ->  9
	sta	$4A8,y
	lda	$780,y		; copy line  7 ->  8
	sta	$428,y
	lda	$700,y		; copy line  6 ->  7
	sta	$780,y
	lda	$680,y		; copy line  5 ->  6
	sta	$700,y
	iny			; y++
				; right side of items
	cpy	#(ttab+length-1)
	bne	scrollloop
	rts

;; double lores addr table

rowlo:	.byte	$00,$00,$80,$80,$00,$00,$80,$80,$00,$00,$80,$80,$00,$00,$80,$80
	.byte	$28,$28,$A8,$A8,$28,$28,$A8,$A8,$28,$28,$A8,$A8,$28,$28,$A8,$A8
	.byte	$50,$50,$D0,$D0,$50,$50,$D0,$D0,$50,$50,$D0,$D0,$50,$50,$D0,$D0
rowhi:	.byte	$4,$4,$4,$4,$5,$5,$5,$5,$6,$6,$6,$6,$7,$7,$7,$7
	.byte	$4,$4,$4,$4,$5,$5,$5,$5,$6,$6,$6,$6,$7,$7,$7,$7
	.byte	$4,$4,$4,$4,$5,$5,$5,$5,$6,$6,$6,$6,$7,$7,$7,$7

;; string constants

title:	.asciiz	"Apple ][ Game Server Online! Client"
titleend:
ins1:	.asciiz	"Use the arrow keys to select then"
ins1end:
ins2:	.asciiz	"[RETURN] to launch."
ins2end:
ins3:	.asciiz	"Connect smartphone headphone jack to Apple //e cassette input jack (next to"
ins3end:
ins4:	.asciiz	"joystick port).  Turn volume to max.  Point QR scanner at screen."
ins4end:
;ins5:	.asciiz	"Visit http://asciiexpress.net/gameserver for more information."
;ins5end:

;; game titles

first:
s001:	.asciiz	"Agent USA            "
s002:	.asciiz	"Air Cars             "
s003:	.asciiz	"Alcazar              "
s004:	.asciiz	"Alien Ambush         "
s005:	.asciiz	"Alien Game           "
s006:	.asciiz	"Alien Munchies       "
s007:	.asciiz	"Alien Typhoon        "
s008:	.asciiz	"Alivader             "
s009:	.asciiz	"Ape Escape           "
s010:	.asciiz	"Apple Oids           "
s011:	.asciiz	"Apple Panic Joystick "
s012:	.asciiz	"Apple Zap            "
s013:	.asciiz	"Aquatron             "
s014:	.asciiz	"Asteroids            "
s015:	.asciiz	"Autobahn             "
s016:	.asciiz	"BC's Quest for Tires "
s017:	.asciiz	"BallBlazer           "
s018:	.asciiz	"Battle Cruiser       "
s019:	.asciiz	"Beach Head           "
s020:	.asciiz	"Bilestoad            "
s021:	.asciiz	"Birth of the Phoenix "
s022:	.asciiz	"Black Flame Pinball  "
s023:	.asciiz	"Blackout             "
s024:	.asciiz	"Blister Ball         "
s025:	.asciiz	"Blitzkrieg           "
s026:	.asciiz	"Bloodsuckers         "
s027:	.asciiz	"Bolo                 "
s028:	.asciiz	"Bug Attack           "
s029:	.asciiz	"Bug Battle           "
s030:	.asciiz	"Burgertime           "
s031:	.asciiz	"Burnout              "
s032:	.asciiz	"Buzzard Bait         "
s033:	.asciiz	"CC Rally             "
s034:	.asciiz	"Cannon Blitz         "
s035:	.asciiz	"Canyon Climber       "
s036:	.asciiz	"Ceiling Zero         "
s037:	.asciiz	"Centipede            "
s038:	.asciiz	"Choplifter           "
s039:	.asciiz	"Collect              "
s040:	.asciiz	"Cosmic Juggler       "
s041:	.asciiz	"County Carnival      "
s042:	.asciiz	"Crazy Climber        "
s043:	.asciiz	"Crimewave            "
s044:	.asciiz	"Crystal Castles      "
s045:	.asciiz	"Cubit                "
s046:	.asciiz	"Cyclod               "
s047:	.asciiz	"Cyclotron            "
s048:	.asciiz	"Dawn Treader         "
s049:	.asciiz	"Deathmaze 5000       "
s050:	.asciiz	"Deep Water Danger    "
s051:	.asciiz	"Defender             "
s052:	.asciiz	"Diamond Mine         "
s053:	.asciiz	"Dig 'em              "
s054:	.asciiz	"Dig Dug              "
s055:	.asciiz	"Dino Eggs            "
s056:	.asciiz	"Dogfight             "
s057:	.asciiz	"Draw Poker           "
s058:	.asciiz	"Drelbs               "
s059:	.asciiz	"Dung Beetles         "
s060:	.asciiz	"Eagle Eggs           "
s061:	.asciiz	"Eliminator           "
s062:	.asciiz	"Exterminator         "
s063:	.asciiz	"Falcons              "
s064:	.asciiz	"Fender Bender        "
s065:	.asciiz	"Fire and Ice         "
s066:	.asciiz	"Firebird             "
s067:	.asciiz	"Flap Smack           "
s068:	.asciiz	"Flicker Color Test   "
s069:	.asciiz	"Flight Simulator     "
s070:	.asciiz	"Formula 1 Racer      "
s071:	.asciiz	"Frazzle              "
s072:	.asciiz	"Free Fall            "
s073:	.asciiz	"Frenzy               "
s074:	.asciiz	"Frogger              "
s075:	.asciiz	"Frontline            "
s076:	.asciiz	"GO                   "
s077:	.asciiz	"Gadgetz              "
s078:	.asciiz	"Galaxian key         "
s079:	.asciiz	"Galaxian             "
s080:	.asciiz	"Galaxy Wars          "
s081:	.asciiz	"Genesis              "
s082:	.asciiz	"Gold Rush            "
s083:	.asciiz	"Grapple              "
s084:	.asciiz	"Guardian             "
s085:	.asciiz	"Hard Hat Mack        "
s086:	.asciiz	"Hardhat              "
s087:	.asciiz	"Head On              "
s088:	.asciiz	"Hell Storm           "
s089:	.asciiz	"Hero                 "
s090:	.asciiz	"Highrise             "
s091:	.asciiz	"Hive Keepers         "
s092:	.asciiz	"Hoe Hopper Pin       "
s093:	.asciiz	"Horizon V            "
s094:	.asciiz	"Hungry Boy           "
s095:	.asciiz	"Hyper Head On        "
s096:	.asciiz	"Interlude            "
s097:	.asciiz	"Jawbreaker           "
s098:	.asciiz	"Jigsaw               "
s099:	.asciiz	"Jupiter Express      "
s100:	.asciiz	"Kingtut Revenge      "
s101:	.asciiz	"Labyrinth            "
s102:	.asciiz	"Laser Silk           "
s103:	.asciiz	"Lemmings             "
s104:	.asciiz	"Livewire Pinball     "
s105:	.asciiz	"Lost Tomb            "
s106:	.asciiz	"Mapple               "
s107:	.asciiz	"Mario Bros           "
s108:	.asciiz	"Mars Cars            "
s109:	.asciiz	"Millenium Leaper     "
s110:	.asciiz	"Millipede            "
s111:	.asciiz	"Mines of Malzeb      "
s112:	.asciiz	"Minit Man            "
s113:	.asciiz	"Money Munchers       "
s114:	.asciiz	"Montezuma's Revenge  "
s115:	.asciiz	"Moon Patrol          "
s116:	.asciiz	"MouskAttack          "
s117:	.asciiz	"Mr. Cool             "
s118:	.asciiz	"Ms. Pacman           "
s119:	.asciiz	"Narnia               "
s120:	.asciiz	"Neptune              "
s121:	.asciiz	"Night Crawler        "
s122:	.asciiz	"Night Driver         "
s123:	.asciiz	"Night Flight         "
s124:	.asciiz	"Night Mission Pinball"
s125:	.asciiz	"Night Survival       "
s126:	.asciiz	"Nightmare Gallery    "
s127:	.asciiz	"Norad                "
s128:	.asciiz	"Odesta Odin          "
s129:	.asciiz	"One-on-One           "
s130:	.asciiz	"Orbitron             "
s131:	.asciiz	"Oriley's Mine        "
s132:	.asciiz	"Outpost              "
s133:	.asciiz	"Outworld             "
s134:	.asciiz	"Pacman               "
s135:	.asciiz	"Palace in Thunderland"
s136:	.asciiz	"Pandora's Box        "
s137:	.asciiz	"Pengo                "
s138:	.asciiz	"Pensate              "
s139:	.asciiz	"People Pong          "
s140:	.asciiz	"Pest Patrol          "
s141:	.asciiz	"Phantom's Five       "
s142:	.asciiz	"Phaser Fire          "
s143:	.asciiz	"Photar               "
s144:	.asciiz	"Pieman               "
s145:	.asciiz	"Pig Pen              "
s146:	.asciiz	"Pipe Dream           "
s147:	.asciiz	"Pirates' Ball        "
s148:	.asciiz	"Pitfall II           "
s149:	.asciiz	"Pooyan               "
s150:	.asciiz	"Precinct Patrol      "
s151:	.asciiz	"Procyon Warrior      "
s152:	.asciiz	"Pulsar               "
s153:	.asciiz	"Quibble's Revenge    "
s154:	.asciiz	"Raster Blaster       "
s155:	.asciiz	"Repton               "
s156:	.asciiz	"Return of Galaxian   "
s157:	.asciiz	"Reversal             "
s158:	.asciiz	"Ribbit               "
s159:	.asciiz	"Rings of Saturn      "
s160:	.asciiz	"Robotron 2084        "
s161:	.asciiz	"Rocket Command       "
s162:	.asciiz	"SYZYGY               "
s163:	.asciiz	"Sabotage             "
s164:	.asciiz	"Shootout at Ok Galaxy"
s165:	.asciiz	"Snack Attack         "
s166:	.asciiz	"Sneakers             "
s167:	.asciiz	"Space Cadet          "
s168:	.asciiz	"Space Eggs           "
s169:	.asciiz	"Space Quarks         "
s170:	.asciiz	"Space Warrior        "
s171:	.asciiz	"Special Sampler      "
s172:	.asciiz	"Spectre              "
s173:	.asciiz	"Spy Hunter           "
s174:	.asciiz	"Spy Strikes Back     "
s175:	.asciiz	"Spy's Demise         "
s176:	.asciiz	"Star Avenger         "
s177:	.asciiz	"Star Blazer          "
s178:	.asciiz	"Star Cruiser         "
s179:	.asciiz	"Star Thief           "
s180:	.asciiz	"Star Trek            "
s181:	.asciiz	"Star Wars Part 2     "
s182:	.asciiz	"Starmaze             "
s183:	.asciiz	"Starmines            "
s184:	.asciiz	"Stellar Invader      "
s185:	.asciiz	"Stunt Cycle          "
s186:	.asciiz	"Succession           "
s187:	.asciiz	"Suicide              "
s188:	.asciiz	"Super Huey           "
s189:	.asciiz	"Super Invader        "
s190:	.asciiz	"Super Othello        "
s191:	.asciiz	"Super Puckman        "
s192:	.asciiz	"Superfalcons         "
s193:	.asciiz	"Swashbuckler         "
s194:	.asciiz	"Taxman               "
s195:	.asciiz	"Technocar Racing     "
s196:	.asciiz	"Tetris               "
s197:	.asciiz	"Threshold            "
s198:	.asciiz	"Thunderbird GX       "
s199:	.asciiz	"Thunderbirds         "
s200:	.asciiz	"Thunderbombs         "
s201:	.asciiz	"Tom Bomb'em          "
s202:	.asciiz	"Torax                "
s203:	.asciiz	"Triad                "
s204:	.asciiz	"Trompers             "
s205:	.asciiz	"Tron                 "
s206:	.asciiz	"Tubeway              "
s207:	.asciiz	"Tumble Bugs          "
s208:	.asciiz	"Tunnel Terror        "
s209:	.asciiz	"Type Attack          "
s210:	.asciiz	"Vindicator           "
s211:	.asciiz	"Vopper               "
s212:	.asciiz	"War Head             "
s213:	.asciiz	"Wargle               "
s214:	.asciiz	"Warlord              "
s215:	.asciiz	"Xevious              "
s216:	.asciiz	"Zargs                "
last:
s217:	.asciiz	"Zenith               "

qr001:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$3C,$50,$6E,$92,$CA,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$0F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$51,$0E,$A1,$FC,$51,$B3,$D0,$60,$EB,$E0,$30,$2A,$2D,$FD,$06,$38
	.byte	$3D,$59,$D6,$99,$AE,$BE,$94,$8F,$32,$82,$0A,$E3,$48,$26,$E2,$B7
	.byte	$DF,$69,$A9,$C3,$59,$39,$55,$29,$2B,$3E,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$46,$43,$19,$BA,$FF,$CF,$B5,$D6,$D4,$80,$6E,$B9
	.byte	$7C,$F5,$05,$57,$1A,$AF,$EE,$F5,$6E,$00

qr002:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$4C,$50,$6E,$84,$CA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$62,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CA,$55
	.byte	$56,$E5,$B1,$FC,$61,$89,$D0,$60,$32,$24,$30,$24,$26,$7D,$06,$24
	.byte	$28,$59,$D4,$4F,$36,$BE,$8A,$1E,$72,$82,$AA,$1F,$48,$26,$CE,$C7
	.byte	$DF,$62,$C2,$43,$59,$01,$B5,$29,$2A,$6E,$7C,$FB,$80,$69,$5C,$7F
	.byte	$FB,$14,$6B,$90,$41,$03,$1B,$BA,$A7,$CF,$B5,$D7,$94,$80,$6E,$B3
	.byte	$7C,$F5,$05,$A7,$1A,$AF,$EB,$D5,$6E,$00

qr003:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$DC,$50,$6E,$9C,$4A,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$56,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$54,$CA,$81,$FC,$74,$B0,$50,$60,$51,$D4,$30,$22,$34,$FD,$06,$5C
	.byte	$DB,$59,$D7,$3B,$96,$BE,$92,$82,$32,$82,$22,$87,$48,$24,$C1,$87
	.byte	$DF,$67,$FA,$43,$59,$32,$E5,$29,$28,$B9,$FC,$FB,$80,$4D,$5C,$7F
	.byte	$FB,$B4,$6B,$90,$4F,$03,$18,$BA,$85,$CF,$B5,$D6,$A4,$80,$6E,$A9
	.byte	$FC,$F5,$05,$13,$1A,$AF,$E8,$75,$6E,$00

qr004:	.byte	33
	.byte	$FE,$F7,$66,$3F,$C1,$48,$A5,$50,$6E,$91,$1D,$4B,$B7,$43,$3E,$E5
	.byte	$DB,$A9,$9C,$42,$EC,$17,$75,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$44
	.byte	$00,$E6,$DD,$99,$F9,$A4,$44,$5C,$F9,$DE,$9B,$AD,$5F,$6B,$B7,$77
	.byte	$74,$06,$FE,$60,$1A,$58,$23,$32,$5D,$BD,$DA,$45,$EC,$E3,$81,$88
	.byte	$33,$90,$67,$3D,$A8,$A4,$FA,$A4,$6D,$BB,$EE,$EB,$A4,$7A,$30,$17
	.byte	$71,$1E,$81,$38,$69,$93,$4A,$89,$31,$CF,$8F,$5A,$C7,$D5,$AA,$73
	.byte	$0B,$45,$D8,$FF,$DD,$09,$F9,$00,$54,$DD,$44,$7F,$9B,$CE,$AA,$30
	.byte	$5B,$41,$31,$9B,$A4,$7A,$BF,$CD,$D3,$3E,$E4,$4E,$EA,$C4,$41,$27
	.byte	$05,$4B,$05,$E8,$FE,$FD,$18,$D4,$80

qr005:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$1C,$50,$6E,$9C,$4A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$52,$4C,$C1,$FC,$71,$8A,$D0,$61,$52,$34,$30,$20,$66,$7D,$06,$4A
	.byte	$2A,$59,$D6,$E8,$1E,$BE,$99,$B5,$32,$82,$2E,$23,$48,$25,$A8,$B7
	.byte	$DF,$6A,$BB,$C3,$59,$20,$E5,$29,$28,$B3,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FA,$1C,$6B,$90,$44,$43,$1A,$BA,$8F,$CF,$A5,$D6,$84,$80,$6E,$A0
	.byte	$7C,$F5,$05,$07,$1A,$AF,$EF,$F5,$6E,$00

qr006:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$0F,$A9,$10,$6E,$95,$8F,$0B,$B7,$46,$68,$B5
	.byte	$DB,$A3,$D7,$62,$EC,$12,$B1,$79,$07,$FA,$AA,$AA,$FE,$01,$94,$25
	.byte	$00,$DA,$4F,$C0,$A0,$E6,$D1,$39,$AC,$A8,$92,$99,$CD,$2B,$B0,$6F
	.byte	$05,$C8,$36,$68,$1A,$58,$AF,$43,$9A,$A1,$FA,$8E,$C8,$71,$A3,$5E
	.byte	$26,$C5,$6B,$B9,$9A,$ED,$86,$98,$BC,$7C,$F8,$B3,$D4,$7A,$30,$8E
	.byte	$82,$02,$FE,$A9,$5D,$01,$04,$EA,$68,$9A,$DA,$DD,$56,$9C,$8F,$53
	.byte	$7A,$82,$C4,$FB,$DD,$29,$F9,$00,$63,$F1,$45,$BF,$87,$74,$EB,$10
	.byte	$46,$24,$71,$DB,$AC,$21,$9F,$C5,$D6,$FE,$F8,$3E,$E8,$C7,$C1,$27
	.byte	$05,$16,$34,$2F,$FE,$AF,$51,$F0,$00

qr007:	.byte	33
	.byte	$FE,$99,$C6,$3F,$C1,$77,$45,$50,$6E,$8E,$FF,$4B,$B7,$44,$4F,$E5
	.byte	$DB,$AF,$72,$42,$EC,$14,$88,$25,$07,$FA,$AA,$AA,$FE,$01,$77,$14
	.byte	$00,$E6,$E6,$11,$F9,$9E,$B3,$2C,$F9,$FA,$84,$45,$5F,$71,$98,$87
	.byte	$74,$0A,$F5,$D8,$1A,$5D,$0C,$46,$5D,$BF,$1D,$3A,$6C,$E3,$D3,$37
	.byte	$73,$90,$3E,$A6,$88,$A4,$CA,$13,$2D,$BB,$E1,$B4,$64,$7A,$3C,$B4
	.byte	$B1,$1E,$80,$A3,$D3,$93,$4B,$EE,$4E,$CF,$8F,$99,$3C,$55,$AA,$4A
	.byte	$34,$05,$D8,$CF,$47,$89,$F9,$00,$43,$DD,$44,$7F,$9C,$5E,$AA,$30
	.byte	$5C,$B5,$31,$BB,$A3,$D0,$BF,$CD,$D0,$4D,$E4,$4E,$EA,$BE,$C1,$27
	.byte	$05,$F7,$05,$E8,$FE,$C6,$B8,$D4,$80

qr008:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$5C,$50,$6E,$84,$4A,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$4F,$AE,$C1,$60,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$53,$A9,$91,$FC,$4F,$E9,$D0,$61,$BA,$20,$30,$2E,$F6,$7D,$06,$14
	.byte	$C8,$59,$D4,$7B,$26,$BE,$95,$9A,$72,$82,$7F,$3F,$48,$27,$20,$C7
	.byte	$DF,$6A,$B2,$C3,$59,$5B,$F1,$29,$2B,$AC,$7C,$FB,$80,$48,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$41,$43,$1B,$BA,$C7,$CF,$B5,$D5,$94,$80,$6E,$A3
	.byte	$7C,$F5,$05,$23,$1A,$AF,$E9,$D5,$6E,$00

qr009:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$0C,$50,$6E,$9C,$4A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DA,$55
	.byte	$54,$60,$F1,$FC,$5D,$AB,$D0,$60,$4A,$34,$30,$28,$AE,$7D,$06,$0A
	.byte	$C8,$59,$D5,$B8,$0E,$BE,$8D,$B9,$72,$82,$2A,$07,$48,$27,$EF,$97
	.byte	$DF,$60,$A3,$43,$59,$3B,$E5,$29,$29,$69,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FA,$14,$6B,$90,$4C,$43,$1A,$BA,$AF,$CF,$A5,$D6,$84,$80,$6E,$AA
	.byte	$7C,$F5,$05,$17,$1A,$AF,$E8,$75,$6E,$00

qr010:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$0C,$50,$6E,$9C,$4A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$54,$C4,$E1,$FC,$64,$8A,$D0,$61,$91,$30,$30,$2C,$36,$7D,$06,$30
	.byte	$28,$59,$D4,$D8,$0E,$BE,$87,$2D,$72,$82,$AF,$83,$48,$25,$6B,$B7
	.byte	$DF,$66,$AB,$C3,$59,$48,$65,$29,$29,$A3,$FC,$FB,$80,$4E,$5C,$7F
	.byte	$FA,$1C,$6B,$90,$4B,$43,$1A,$BA,$A7,$CF,$A5,$D6,$A4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$C7,$1A,$AF,$EC,$F5,$6E,$00

qr011:	.byte	33
	.byte	$FE,$21,$73,$3F,$C1,$73,$D6,$D0,$6E,$A0,$D8,$4B,$B7,$49,$46,$45
	.byte	$DB,$AE,$F6,$32,$EC,$15,$78,$C5,$07,$FA,$AA,$AA,$FE,$01,$65,$CA
	.byte	$00,$D3,$1E,$0D,$BB,$42,$6F,$66,$53,$5A,$86,$3C,$98,$7B,$3F,$E4
	.byte	$FA,$3A,$AB,$21,$4F,$09,$A0,$A1,$65,$5C,$68,$5A,$3D,$25,$AB,$E1
	.byte	$F9,$3A,$F3,$4D,$5F,$B8,$BC,$87,$23,$83,$15,$9E,$9B,$2F,$77,$15
	.byte	$59,$FD,$0B,$F6,$19,$54,$51,$EA,$94,$65,$27,$CD,$03,$69,$DA,$BB
	.byte	$84,$7D,$3B,$8B,$88,$C4,$FC,$00,$6C,$1E,$C6,$7F,$AA,$19,$AA,$50
	.byte	$45,$EE,$91,$3B,$A7,$75,$4F,$85,$D4,$05,$07,$C2,$E8,$13,$94,$73
	.byte	$05,$2A,$CB,$D0,$FE,$DA,$04,$A5,$00

qr012:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$0C,$50,$6E,$92,$4A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$50,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$50,$86,$A1,$FC,$76,$B2,$D0,$60,$A3,$E0,$30,$2B,$AD,$FD,$06,$34
	.byte	$1C,$59,$D5,$49,$AE,$BE,$98,$0F,$72,$82,$23,$A3,$48,$26,$28,$B7
	.byte	$DF,$68,$91,$C3,$59,$00,$95,$29,$2B,$34,$7C,$FB,$80,$4B,$5C,$7F
	.byte	$FA,$34,$6B,$90,$4A,$03,$19,$BA,$DF,$CF,$B5,$D4,$C4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$57,$1A,$AF,$EC,$75,$6E,$00

qr013:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$4C,$50,$6E,$84,$CA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$51,$81,$81,$FC,$74,$C8,$50,$61,$41,$24,$30,$22,$A6,$7D,$06,$38
	.byte	$A9,$59,$D5,$FE,$2E,$BE,$99,$B6,$32,$82,$EE,$FF,$48,$25,$04,$D7
	.byte	$DF,$65,$B3,$43,$59,$70,$F1,$29,$29,$6C,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$9C,$6B,$90,$45,$43,$1B,$BA,$E7,$CF,$B5,$D4,$A4,$80,$6E,$B3
	.byte	$FC,$F5,$05,$63,$1A,$AF,$EF,$D5,$6E,$00

qr014:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$2C,$50,$6E,$92,$CA,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$55,$AA,$81,$FC,$47,$B2,$D0,$60,$13,$E4,$30,$28,$35,$FD,$06,$3E
	.byte	$DD,$59,$D4,$B9,$36,$BE,$95,$87,$32,$82,$2A,$E7,$48,$24,$65,$A7
	.byte	$DF,$66,$99,$C3,$59,$08,$15,$29,$2A,$6A,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$24,$6B,$90,$42,$43,$19,$BA,$DF,$CF,$B5,$D5,$C4,$80,$6E,$A8
	.byte	$FC,$F5,$05,$F3,$1A,$AF,$E8,$75,$6E,$00

qr015:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$5C,$50,$6E,$84,$CA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$51,$2D,$A1,$FC,$71,$E9,$D0,$60,$40,$24,$30,$2C,$2E,$7D,$06,$00
	.byte	$C8,$59,$D7,$AF,$36,$BE,$95,$2E,$32,$82,$E3,$3F,$48,$24,$47,$C7
	.byte	$DF,$67,$DA,$43,$59,$01,$75,$29,$2B,$FA,$7C,$FB,$80,$7A,$5C,$7F
	.byte	$FB,$04,$6B,$90,$41,$03,$1B,$BA,$E7,$CF,$B5,$D4,$A4,$80,$6E,$A3
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$E9,$D5,$6E,$00

qr016:	.byte	33
	.byte	$FE,$85,$81,$3F,$C1,$4C,$39,$10,$6E,$B2,$9B,$6B,$B7,$5D,$D4,$05
	.byte	$DB,$A6,$F0,$32,$EC,$17,$30,$A1,$07,$FA,$AA,$AA,$FE,$00,$65,$9A
	.byte	$00,$CE,$28,$96,$97,$96,$02,$E0,$88,$32,$D9,$CB,$67,$90,$39,$3C
	.byte	$97,$88,$A9,$6D,$6B,$98,$CB,$5D,$1A,$A3,$9A,$FE,$AF,$6D,$D9,$28
	.byte	$BD,$A8,$8B,$CD,$47,$B8,$AC,$A3,$B9,$CA,$2D,$F8,$4A,$42,$D8,$BD
	.byte	$5E,$FD,$05,$FD,$6D,$62,$8C,$49,$4C,$08,$90,$AE,$7C,$36,$24,$13
	.byte	$2A,$BB,$E0,$CB,$6C,$1E,$FD,$00,$63,$E5,$45,$BF,$90,$6A,$AA,$D0
	.byte	$51,$5E,$D1,$1B,$AD,$6D,$CF,$85,$D3,$4C,$23,$52,$E8,$7D,$82,$A9
	.byte	$05,$AB,$AB,$D0,$FE,$ED,$5F,$C8,$80

qr017:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$08,$50,$6E,$9C,$8A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$55,$A4,$E1,$FC,$4F,$EA,$D0,$61,$78,$30,$30,$27,$76,$7D,$06,$66
	.byte	$A8,$59,$D5,$5E,$96,$BE,$9E,$31,$32,$82,$CE,$87,$48,$26,$6F,$A7
	.byte	$DF,$67,$EA,$C3,$59,$30,$21,$29,$29,$3F,$FC,$FB,$80,$6C,$5C,$7F
	.byte	$FB,$A4,$6B,$90,$4F,$43,$1A,$BA,$A7,$CF,$A5,$D7,$B4,$80,$6E,$B9
	.byte	$7C,$F5,$05,$43,$1A,$AF,$EE,$F5,$6E,$00

qr018:	.byte	33
	.byte	$FE,$77,$46,$3F,$C1,$0C,$B5,$10,$6E,$9C,$AD,$8B,$B7,$56,$69,$B5
	.byte	$DB,$AA,$F1,$F2,$EC,$13,$34,$61,$07,$FA,$AA,$AA,$FE,$00,$98,$35
	.byte	$00,$C7,$6B,$5A,$8C,$52,$91,$31,$AC,$8A,$96,$0B,$84,$0C,$A6,$71
	.byte	$64,$46,$B6,$69,$9A,$5F,$EB,$70,$D9,$AF,$49,$1D,$81,$55,$B0,$DE
	.byte	$06,$C5,$0E,$8B,$D3,$C9,$5C,$D0,$DD,$FA,$D7,$A3,$D6,$7A,$25,$BA
	.byte	$41,$0E,$C6,$BF,$14,$A5,$96,$68,$68,$9A,$DA,$5B,$1F,$98,$1D,$3B
	.byte	$1B,$54,$DC,$D7,$7D,$31,$F9,$00,$50,$FD,$45,$7F,$B6,$52,$6B,$50
	.byte	$5A,$25,$71,$DB,$A1,$04,$0F,$8D,$D1,$78,$E0,$5E,$E8,$47,$E1,$27
	.byte	$05,$DA,$24,$EC,$FE,$8B,$C3,$B9,$00

qr019:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$18,$50,$6E,$9C,$8A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$54,$E4,$C1,$FC,$55,$AB,$50,$61,$A9,$34,$30,$24,$A6,$7D,$06,$1A
	.byte	$6A,$59,$D5,$7A,$06,$BE,$99,$2D,$32,$82,$0E,$A3,$48,$27,$4A,$B7
	.byte	$DF,$62,$EB,$C3,$59,$50,$25,$29,$2A,$A3,$FC,$FB,$80,$6E,$5C,$7F
	.byte	$FA,$24,$6B,$90,$44,$03,$1A,$BA,$EF,$CF,$A5,$D6,$84,$80,$6E,$A8
	.byte	$FC,$F5,$05,$D7,$1A,$AF,$EE,$F5,$6E,$00

qr020:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$08,$50,$6E,$92,$8A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$53,$8E,$81,$FC,$6B,$B2,$D0,$61,$EA,$E4,$30,$27,$25,$FD,$06,$3A
	.byte	$BD,$59,$D6,$6C,$36,$BE,$81,$8F,$72,$82,$CA,$07,$48,$26,$62,$97
	.byte	$DF,$68,$E8,$C3,$59,$0A,$D1,$29,$29,$B4,$7C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$4E,$43,$19,$BA,$FF,$CF,$B5,$D6,$C4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$07,$1A,$AF,$EE,$F5,$6E,$00

qr021:	.byte	33
	.byte	$FE,$42,$CD,$3F,$C1,$6F,$A3,$10,$6E,$9C,$A7,$8B,$B7,$5A,$C8,$75
	.byte	$DB,$A2,$81,$F2,$EC,$15,$08,$41,$07,$FA,$AA,$AA,$FE,$00,$14,$7D
	.byte	$00,$FB,$EF,$D2,$D5,$52,$E1,$7E,$B0,$F0,$D7,$EB,$84,$06,$1E,$27
	.byte	$E6,$4A,$B5,$1D,$AC,$87,$C5,$65,$79,$2F,$18,$1C,$01,$55,$78,$19
	.byte	$0A,$B4,$5E,$4A,$13,$C9,$24,$70,$2F,$F2,$C8,$9E,$50,$A1,$59,$B2
	.byte	$61,$8C,$C1,$37,$0C,$A5,$95,$28,$74,$EB,$1E,$AC,$1E,$98,$1D,$08
	.byte	$DA,$6C,$FC,$8A,$AA,$4A,$FC,$80,$60,$7F,$45,$7F,$A6,$4A,$6B,$50
	.byte	$42,$54,$B1,$DB,$AF,$05,$0F,$9D,$D4,$77,$C0,$DE,$EB,$9F,$0C,$91
	.byte	$05,$9B,$2C,$CC,$FE,$AB,$43,$B9,$00

qr022:	.byte	33
	.byte	$FE,$28,$BD,$3F,$C1,$50,$D3,$10,$6E,$82,$A3,$8B,$B7,$5D,$FA,$75
	.byte	$DB,$A4,$2F,$F2,$EC,$16,$F8,$01,$07,$FA,$AA,$AA,$FE,$00,$E1,$3D
	.byte	$00,$FB,$D0,$42,$D5,$2E,$D6,$1E,$B0,$EA,$E9,$C3,$84,$09,$91,$83
	.byte	$E6,$4B,$2E,$B9,$AC,$82,$84,$06,$79,$2F,$7D,$E7,$21,$54,$B0,$A6
	.byte	$2A,$B4,$BE,$11,$D3,$C9,$4E,$E7,$4F,$F2,$E4,$91,$CA,$A1,$5F,$21
	.byte	$96,$8C,$C8,$B2,$A4,$25,$97,$08,$03,$EB,$1E,$EC,$E4,$B8,$1D,$42
	.byte	$27,$0C,$FC,$B6,$50,$42,$FC,$80,$67,$7F,$45,$7F,$B9,$D0,$6B,$50
	.byte	$41,$B5,$B1,$EB,$AE,$A5,$0F,$9D,$D6,$05,$C0,$DE,$EA,$E7,$8C,$91
	.byte	$05,$E6,$6C,$CC,$FE,$D1,$43,$B9,$00

qr023:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$58,$50,$6E,$84,$0A,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$60,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$50,$25,$81,$FC,$54,$89,$50,$60,$A1,$24,$30,$27,$66,$7D,$06,$44
	.byte	$2A,$59,$D7,$EA,$36,$BE,$8C,$AA,$72,$82,$27,$DF,$48,$25,$AD,$E7
	.byte	$DF,$66,$8B,$C3,$59,$18,$75,$29,$29,$AE,$7C,$FB,$80,$49,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$45,$43,$1B,$BA,$A7,$CF,$B5,$D7,$94,$80,$6E,$A2
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$EB,$D5,$6E,$00

qr024:	.byte	33
	.byte	$FE,$41,$BD,$3F,$C1,$6C,$37,$10,$6E,$9C,$AF,$8B,$B7,$5A,$1B,$75
	.byte	$DB,$A2,$F2,$F2,$EC,$15,$3D,$01,$07,$FA,$AA,$AA,$FE,$00,$1A,$2D
	.byte	$00,$FB,$EB,$4A,$D5,$14,$60,$FE,$B0,$C8,$96,$03,$84,$02,$BE,$55
	.byte	$E6,$4F,$ED,$0C,$2C,$85,$09,$7B,$F9,$2D,$99,$9E,$01,$55,$88,$9A
	.byte	$5A,$B4,$DF,$EB,$6B,$C9,$4E,$50,$FF,$F2,$D4,$8E,$12,$A1,$4F,$12
	.byte	$56,$8C,$C1,$AF,$04,$A5,$97,$CE,$78,$EB,$1E,$48,$9C,$B8,$1D,$00
	.byte	$D9,$0C,$FC,$9B,$0B,$D2,$FC,$80,$70,$4F,$45,$7F,$A6,$78,$6B,$50
	.byte	$4E,$65,$B1,$DB,$A9,$17,$0F,$95,$D7,$77,$C0,$DE,$EB,$1F,$0C,$91
	.byte	$05,$D9,$6C,$CC,$FE,$EB,$C3,$B9,$00

qr025:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$18,$50,$6E,$9C,$0A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$56,$A8,$D1,$FC,$7A,$8A,$D0,$61,$5B,$30,$30,$2C,$6E,$7D,$06,$00
	.byte	$AB,$59,$D4,$6D,$96,$BE,$91,$9D,$32,$82,$3E,$27,$48,$27,$00,$A7
	.byte	$DF,$6D,$AB,$43,$59,$7B,$E5,$29,$2B,$31,$FC,$FB,$80,$7D,$5C,$7F
	.byte	$FA,$14,$6B,$90,$4E,$03,$1A,$BA,$CF,$CF,$A5,$D5,$A4,$80,$6E,$A0
	.byte	$7C,$F5,$05,$47,$1A,$AF,$EE,$F5,$6E,$00

qr026:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$48,$A5,$50,$6E,$91,$13,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9E,$42,$EC,$17,$75,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$54
	.byte	$00,$E6,$DD,$91,$F9,$B2,$84,$5C,$F9,$EB,$DB,$AD,$5F,$6C,$87,$72
	.byte	$74,$0C,$76,$61,$1A,$5C,$E5,$31,$DD,$BF,$D9,$C4,$EC,$E3,$D9,$88
	.byte	$53,$90,$D2,$3D,$B8,$A4,$94,$64,$6D,$BB,$DB,$CB,$A4,$7A,$27,$B3
	.byte	$73,$1E,$89,$B8,$68,$93,$4A,$C7,$31,$CF,$8F,$8B,$C7,$D5,$AA,$7B
	.byte	$8B,$75,$D8,$DB,$FD,$09,$F9,$00,$64,$DD,$44,$7F,$9B,$CE,$AA,$30
	.byte	$5F,$41,$31,$9B,$A4,$7A,$BF,$CD,$D0,$3E,$E4,$4E,$EA,$44,$61,$27
	.byte	$05,$CB,$25,$E8,$FE,$DD,$18,$D4,$80

qr027:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$38,$50,$6E,$82,$8A,$BB,$75,$DE,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$D6,$55
	.byte	$53,$41,$61,$FC,$49,$C9,$D0,$60,$53,$14,$30,$2A,$B6,$DD,$06,$5A
	.byte	$62,$59,$D6,$CC,$FE,$BE,$9A,$BA,$32,$82,$D7,$FF,$48,$24,$62,$57
	.byte	$DF,$62,$D2,$43,$59,$32,$A5,$29,$2A,$3E,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FB,$94,$6B,$90,$4D,$03,$1B,$BA,$ED,$CF,$AD,$D4,$A4,$80,$EE,$A9
	.byte	$FC,$F5,$05,$A3,$1A,$AF,$EC,$75,$6E,$00

qr028:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$18,$50,$6E,$9C,$8A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DA,$55
	.byte	$57,$A0,$F1,$FC,$68,$AB,$D0,$61,$F9,$34,$30,$24,$F6,$7D,$06,$00
	.byte	$C8,$59,$D6,$3E,$86,$BE,$83,$95,$72,$82,$03,$27,$48,$24,$21,$97
	.byte	$DF,$6B,$EA,$C3,$59,$02,$A1,$29,$28,$F9,$FC,$FB,$80,$5D,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$43,$03,$1A,$BA,$9F,$CF,$A5,$D7,$84,$80,$6E,$A0
	.byte	$7C,$F5,$05,$F7,$1A,$AF,$E8,$F5,$6E,$00

qr029:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$18,$50,$6E,$9C,$8A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DA,$55
	.byte	$53,$2C,$F1,$FC,$5A,$CB,$D0,$61,$D2,$34,$30,$28,$3E,$7D,$06,$56
	.byte	$A8,$59,$D4,$BD,$16,$BE,$8F,$89,$32,$82,$D3,$87,$48,$26,$0D,$87
	.byte	$DF,$6F,$BA,$43,$59,$49,$A5,$29,$28,$27,$FC,$FB,$80,$4D,$5C,$7F
	.byte	$FA,$0C,$6B,$90,$40,$03,$1A,$BA,$EF,$CF,$A5,$D5,$94,$80,$6E,$AA
	.byte	$FC,$F5,$05,$83,$1A,$AF,$EF,$F5,$6E,$00

qr030:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$18,$50,$6E,$9C,$8A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$56,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$54,$24,$C1,$FC,$65,$AB,$D0,$61,$AA,$34,$30,$2E,$A6,$7D,$06,$30
	.byte	$28,$59,$D5,$68,$8E,$BE,$8F,$AD,$72,$82,$83,$27,$48,$25,$80,$97
	.byte	$DF,$65,$93,$43,$59,$58,$A1,$29,$29,$7B,$FC,$FB,$80,$7D,$5C,$7F
	.byte	$FA,$0C,$6B,$90,$4C,$03,$1A,$BA,$EF,$CF,$A5,$D6,$A4,$80,$6E,$A0
	.byte	$7C,$F5,$05,$87,$1A,$AF,$EF,$F5,$6E,$00

qr031:	.byte	29
	.byte	$FE,$F4,$43,$FC,$14,$91,$50,$6E,$91,$3C,$BB,$74,$2C,$85,$DB,$A9
	.byte	$C2,$2E,$C1,$72,$71,$07,$FA,$AA,$AF,$E0,$1B,$37,$00,$E6,$D0,$8F
	.byte	$9C,$88,$E8,$D8,$D1,$DC,$66,$BA,$DB,$42,$79,$0C,$AF,$90,$B0,$84
	.byte	$1E,$CB,$9E,$98,$C5,$D3,$35,$97,$56,$10,$DF,$D1,$93,$48,$02,$DE
	.byte	$FB,$FE,$AF,$75,$82,$21,$B7,$60,$0C,$BC,$91,$FD,$00,$79,$CC,$77
	.byte	$F9,$F7,$2A,$30,$5E,$27,$1A,$BA,$13,$1F,$DD,$D1,$ED,$A4,$EE,$BC
	.byte	$4A,$2F,$05,$41,$53,$8F,$ED,$18,$D8,$80

qr032:	.byte	33
	.byte	$FE,$F7,$66,$3F,$C1,$48,$A5,$50,$6E,$91,$19,$4B,$B7,$43,$3F,$E5
	.byte	$DB,$A9,$9F,$42,$EC,$17,$74,$25,$07,$FA,$AA,$AA,$FE,$01,$88,$54
	.byte	$00,$E6,$DD,$99,$F9,$AC,$44,$6C,$F9,$E2,$DB,$BF,$5F,$7B,$A7,$76
	.byte	$74,$06,$A6,$61,$1A,$59,$29,$31,$DD,$BE,$BE,$45,$4C,$E3,$20,$88
	.byte	$03,$90,$1B,$1D,$B8,$A4,$D4,$D4,$6D,$BB,$CE,$B3,$A6,$7A,$35,$23
	.byte	$72,$1E,$81,$60,$68,$13,$4B,$8B,$31,$CF,$8F,$BB,$47,$F5,$AA,$10
	.byte	$CB,$65,$D8,$DB,$9D,$01,$F9,$00,$54,$DD,$44,$7F,$8B,$CC,$AA,$30
	.byte	$5B,$41,$31,$9B,$A0,$7B,$3F,$CD,$D2,$3E,$E4,$4E,$EA,$C4,$61,$27
	.byte	$05,$0B,$25,$E8,$FE,$DD,$18,$D4,$80

qr033:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$6C,$50,$6E,$84,$8A,$BB,$75,$C6,$C5,$DB,$A5
	.byte	$6F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$56,$89,$91,$FC,$4B,$C9,$50,$61,$E3,$20,$30,$26,$AE,$7D,$06,$48
	.byte	$E8,$59,$D5,$A8,$26,$BE,$97,$8A,$72,$82,$7B,$DF,$48,$26,$6A,$E7
	.byte	$DF,$61,$A2,$C3,$59,$21,$31,$29,$2A,$74,$7C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$49,$03,$1B,$BA,$E7,$CF,$B5,$D4,$B4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$27,$1A,$AF,$E9,$D5,$6E,$00

qr034:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$0F,$8D,$10,$6E,$95,$89,$0B,$B7,$46,$69,$B5
	.byte	$DB,$A3,$D3,$62,$EC,$12,$B3,$79,$07,$FA,$AA,$AA,$FE,$01,$94,$05
	.byte	$00,$DA,$4F,$C8,$A0,$9A,$D1,$01,$AC,$B0,$92,$93,$CD,$35,$90,$6F
	.byte	$05,$C1,$E6,$60,$9A,$5F,$2D,$42,$1A,$A3,$78,$8D,$E8,$70,$11,$9D
	.byte	$66,$C5,$A2,$19,$3A,$ED,$94,$E8,$1C,$7C,$F9,$B3,$A6,$7A,$34,$BA
	.byte	$B4,$02,$FC,$35,$4C,$01,$05,$CE,$64,$9A,$DA,$7C,$D5,$BC,$8F,$7B
	.byte	$BA,$B2,$C4,$CB,$3D,$09,$F9,$00,$63,$C1,$45,$BF,$87,$5E,$EB,$10
	.byte	$4A,$15,$71,$CB,$AA,$32,$9F,$DD,$D7,$F9,$F8,$3E,$E9,$C4,$41,$27
	.byte	$05,$17,$54,$2F,$FE,$AF,$51,$F0,$00

qr035:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$48,$95,$50,$6E,$91,$1B,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9A,$42,$EC,$17,$74,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$74
	.byte	$00,$E6,$DD,$81,$F9,$E4,$84,$64,$F9,$CD,$DB,$B7,$5F,$6F,$27,$77
	.byte	$74,$02,$E6,$68,$9A,$5C,$CD,$30,$DD,$BF,$28,$C6,$CC,$E3,$70,$4B
	.byte	$33,$90,$93,$3D,$08,$A4,$F6,$E4,$CD,$BB,$DC,$A3,$D4,$7A,$3E,$BF
	.byte	$44,$1E,$88,$60,$78,$13,$4B,$C3,$3D,$CF,$8F,$DD,$C4,$F5,$AA,$71
	.byte	$0B,$35,$D8,$F7,$7D,$29,$F9,$00,$44,$ED,$44,$7F,$9B,$E4,$AA,$30
	.byte	$53,$70,$31,$8B,$A6,$69,$BF,$D5,$D3,$39,$E4,$4E,$EB,$47,$C1,$27
	.byte	$05,$4A,$65,$E8,$FE,$BD,$18,$D4,$80

qr036:	.byte	33
	.byte	$FE,$41,$DD,$3F,$C1,$6C,$37,$10,$6E,$9C,$AF,$8B,$B7,$5A,$1B,$75
	.byte	$DB,$A2,$F2,$F2,$EC,$15,$3C,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$3D
	.byte	$00,$FB,$EB,$42,$D5,$20,$20,$CE,$B0,$DE,$D6,$19,$84,$1F,$3E,$57
	.byte	$E6,$47,$75,$0D,$2C,$86,$21,$7B,$F9,$2E,$8A,$9F,$21,$55,$99,$1A
	.byte	$0A,$B4,$DF,$6B,$73,$C9,$70,$30,$FF,$F2,$E9,$AE,$10,$A1,$40,$8A
	.byte	$55,$8C,$CB,$F1,$05,$25,$96,$21,$78,$EB,$1E,$AE,$9C,$B8,$1D,$6A
	.byte	$59,$0C,$FC,$AE,$2B,$D2,$FC,$80,$60,$4F,$45,$7F,$A6,$7A,$6B,$50
	.byte	$4A,$65,$B1,$DB,$AD,$16,$0F,$95,$D6,$77,$C0,$DE,$EA,$1F,$2C,$91
	.byte	$05,$19,$6C,$CC,$FE,$8B,$C3,$B9,$00

qr037:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$1C,$50,$6E,$92,$8A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$53,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$51,$62,$91,$FC,$4D,$92,$D0,$61,$E1,$E0,$30,$20,$35,$FD,$06,$26
	.byte	$BE,$59,$D6,$2A,$A6,$BE,$8A,$AB,$72,$82,$3E,$E7,$48,$25,$69,$97
	.byte	$DF,$66,$80,$43,$59,$4B,$51,$29,$2A,$B4,$7C,$FB,$80,$58,$5C,$7F
	.byte	$FB,$2C,$6B,$90,$42,$43,$19,$BA,$BF,$CF,$B5,$D6,$C4,$80,$6E,$A8
	.byte	$FC,$F5,$05,$03,$1A,$AF,$E8,$F5,$6E,$00

qr038:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$0C,$50,$6E,$9C,$0A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$50,$E4,$D1,$FC,$56,$EA,$50,$61,$83,$30,$30,$25,$26,$7D,$06,$7A
	.byte	$EA,$59,$D5,$6E,$8E,$BE,$8E,$05,$32,$82,$F7,$27,$48,$24,$4A,$A7
	.byte	$DF,$6B,$CA,$43,$59,$6B,$A5,$29,$2B,$23,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FB,$BC,$6B,$90,$47,$03,$1A,$BA,$87,$CF,$A5,$D7,$94,$80,$6E,$B8
	.byte	$FC,$F5,$05,$83,$1A,$AF,$EE,$F5,$6E,$00

qr039:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$FC,$50,$6E,$9C,$8A,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$55,$CE,$A1,$FC,$5F,$90,$D0,$61,$63,$D0,$30,$27,$34,$FD,$06,$60
	.byte	$18,$59,$D5,$2B,$8E,$BE,$95,$B6,$72,$82,$C6,$A7,$48,$27,$EC,$87
	.byte	$DF,$6E,$C3,$43,$59,$00,$A5,$29,$2B,$A5,$FC,$FB,$80,$6D,$5C,$7F
	.byte	$FB,$AC,$6B,$90,$47,$03,$18,$BA,$C5,$CF,$B5,$D6,$A4,$80,$6E,$A9
	.byte	$FC,$F5,$05,$93,$1A,$AF,$E8,$75,$6E,$00

qr040:	.byte	33
	.byte	$FE,$F7,$46,$3F,$C1,$48,$91,$50,$6E,$91,$13,$4B,$B7,$43,$3E,$E5
	.byte	$DB,$A9,$9E,$42,$EC,$17,$77,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$74
	.byte	$00,$E6,$DD,$81,$F9,$DA,$84,$44,$F9,$EB,$9B,$A5,$5F,$78,$07,$76
	.byte	$74,$08,$7E,$69,$9A,$5D,$A3,$32,$DD,$BE,$0F,$C7,$4C,$E3,$59,$8B
	.byte	$03,$90,$03,$1D,$08,$A4,$DC,$64,$CD,$BB,$F0,$EB,$D4,$7A,$2C,$2F
	.byte	$45,$1E,$8E,$E0,$78,$93,$48,$0D,$3D,$CF,$8F,$0B,$44,$D5,$AA,$20
	.byte	$CB,$25,$D8,$D2,$3D,$31,$F9,$00,$74,$ED,$44,$7F,$8B,$E4,$AA,$30
	.byte	$57,$73,$31,$8B,$A0,$68,$BF,$D5,$D1,$39,$E4,$4E,$EB,$C7,$C1,$27
	.byte	$05,$4A,$65,$E8,$FE,$9D,$18,$D4,$80

qr041:	.byte	33
	.byte	$FE,$99,$E6,$3F,$C1,$77,$45,$50,$6E,$8E,$F3,$4B,$B7,$44,$4C,$E5
	.byte	$DB,$AF,$74,$42,$EC,$14,$8A,$65,$07,$FA,$AA,$AA,$FE,$01,$77,$34
	.byte	$00,$E6,$E6,$81,$F9,$84,$73,$24,$F9,$F2,$E4,$65,$5F,$64,$08,$B0
	.byte	$74,$08,$35,$D0,$1A,$5F,$CC,$4E,$DD,$BD,$A9,$3D,$EC,$E2,$C0,$34
	.byte	$53,$90,$A3,$07,$90,$A4,$C0,$03,$DD,$BB,$D2,$FC,$46,$7A,$30,$A8
	.byte	$B3,$1E,$85,$77,$D0,$93,$48,$8C,$4C,$CF,$8F,$39,$BF,$D5,$AA,$2B
	.byte	$37,$55,$D8,$DB,$46,$91,$F9,$00,$43,$AD,$44,$7F,$8C,$74,$AA,$30
	.byte	$5C,$A3,$31,$AB,$A7,$D2,$BF,$D5,$D1,$4E,$E4,$4E,$EB,$BC,$41,$27
	.byte	$05,$B4,$25,$E8,$FE,$A7,$18,$D4,$80

qr042:	.byte	33
	.byte	$FE,$2F,$7D,$3F,$C1,$53,$D3,$10,$6E,$83,$43,$8B,$B7,$5D,$6A,$75
	.byte	$DB,$A4,$1F,$F2,$EC,$16,$C0,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$2D
	.byte	$00,$FB,$D0,$C2,$D5,$62,$D7,$9E,$B0,$C1,$C9,$FB,$84,$16,$21,$A7
	.byte	$E6,$43,$B6,$B5,$AC,$82,$0A,$0D,$79,$2F,$DA,$E0,$21,$55,$A2,$25
	.byte	$0A,$B4,$23,$D0,$5B,$C9,$10,$27,$8F,$F2,$D7,$C9,$D8,$A1,$58,$15
	.byte	$96,$8C,$CA,$B4,$BF,$A5,$95,$EE,$07,$EB,$1E,$0B,$67,$38,$1D,$52
	.byte	$A6,$4C,$FC,$8B,$B1,$42,$FC,$80,$67,$4F,$45,$7F,$B9,$E8,$6B,$50
	.byte	$4D,$92,$B1,$FB,$AE,$BC,$8F,$95,$D7,$04,$C0,$DE,$EB,$E5,$AC,$91
	.byte	$05,$A5,$6C,$CC,$FE,$D0,$63,$B9,$00

qr043:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$2C,$50,$6E,$92,$0A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$50,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$50,$EE,$A1,$FC,$4B,$D2,$D0,$61,$1A,$E4,$30,$2C,$F5,$FD,$06,$58
	.byte	$9C,$59,$D5,$CA,$AE,$BE,$81,$3B,$72,$82,$6E,$47,$48,$25,$C9,$97
	.byte	$DF,$63,$D9,$C3,$59,$6A,$95,$29,$28,$34,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FA,$A4,$6B,$90,$46,$43,$19,$BA,$9F,$CF,$B5,$D5,$D4,$80,$6E,$B1
	.byte	$FC,$F5,$05,$43,$1A,$AF,$EA,$F5,$6E,$00

qr044:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$F7,$10,$6E,$83,$47,$8B,$B7,$5D,$6B,$75
	.byte	$DB,$A4,$1A,$F2,$EC,$16,$C1,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$1D
	.byte	$00,$FB,$D0,$52,$D5,$46,$57,$8E,$B0,$F6,$89,$DB,$84,$04,$11,$90
	.byte	$E6,$4E,$A6,$BC,$2C,$87,$4A,$04,$F9,$2E,$2E,$E6,$01,$54,$18,$66
	.byte	$5A,$B4,$DB,$B1,$4B,$C9,$48,$B7,$7F,$F2,$F5,$89,$E2,$A1,$46,$25
	.byte	$96,$8C,$C5,$BA,$BC,$A5,$97,$64,$05,$EB,$1E,$5D,$64,$38,$1D,$6B
	.byte	$65,$0C,$FC,$92,$B0,$5A,$FC,$80,$77,$0F,$45,$7F,$B9,$D8,$6B,$50
	.byte	$41,$82,$B1,$EB,$AC,$BE,$8F,$8D,$D4,$07,$C0,$DE,$EA,$E7,$2C,$91
	.byte	$05,$26,$6C,$CC,$FE,$F1,$C3,$B9,$00

qr045:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$DC,$50,$6E,$9C,$8A,$BB,$75,$AE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$01,$13,$00,$FB,$EE,$55
	.byte	$55,$A6,$21,$FC,$57,$96,$50,$60,$09,$E4,$30,$2A,$E5,$FD,$06,$50
	.byte	$F5,$59,$D4,$AE,$2E,$BE,$88,$12,$32,$82,$D6,$45,$48,$27,$21,$A7
	.byte	$DF,$6D,$B1,$C3,$59,$3B,$91,$29,$28,$A6,$FC,$FB,$80,$59,$5C,$7F
	.byte	$FA,$04,$6B,$90,$46,$43,$19,$BA,$E7,$CF,$AD,$D7,$94,$80,$EE,$AB
	.byte	$7C,$F5,$05,$23,$1A,$AF,$EF,$D5,$6E,$00

qr046:	.byte	29
	.byte	$FE,$2E,$9B,$FC,$15,$0C,$50,$6E,$82,$8A,$BB,$75,$DE,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6E,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$51,$21,$61,$FC,$7A,$CA,$D0,$60,$F9,$20,$30,$26,$36,$7D,$06,$40
	.byte	$2A,$59,$D5,$29,$1E,$BE,$80,$99,$72,$82,$9A,$2F,$48,$27,$69,$87
	.byte	$DF,$63,$F0,$C3,$59,$09,$A1,$29,$28,$38,$5C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$4E,$03,$1A,$BA,$BF,$CF,$B5,$D6,$D4,$80,$6E,$AA
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EA,$75,$6E,$00

qr047:	.byte	29
	.byte	$FE,$76,$9B,$FC,$15,$0C,$50,$6E,$92,$8A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$0F,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$54,$C6,$A1,$FC,$73,$F2,$D0,$61,$0A,$E0,$30,$23,$AD,$FD,$06,$32
	.byte	$7E,$59,$D4,$49,$BE,$BE,$8A,$BF,$72,$82,$8E,$27,$48,$26,$66,$97
	.byte	$DF,$65,$B0,$C3,$59,$6B,$D1,$29,$28,$38,$7C,$FB,$80,$49,$5C,$7F
	.byte	$FB,$34,$6B,$90,$46,$03,$19,$BA,$FF,$CF,$B5,$D6,$E4,$80,$6E,$B3
	.byte	$FC,$F5,$05,$E7,$1A,$AF,$ED,$F5,$6E,$00

qr048:	.byte	33
	.byte	$FE,$F7,$66,$3F,$C1,$48,$85,$50,$6E,$91,$15,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9E,$42,$EC,$17,$75,$25,$07,$FA,$AA,$AA,$FE,$01,$88,$24
	.byte	$00,$E6,$DD,$91,$F9,$C0,$C4,$4C,$F9,$E4,$DB,$B7,$5F,$66,$B7,$75
	.byte	$74,$06,$FE,$61,$1A,$58,$6D,$31,$DD,$BF,$9A,$C4,$CC,$E3,$93,$48
	.byte	$33,$90,$DA,$7D,$A0,$A4,$D6,$44,$65,$BB,$D4,$8B,$A6,$7A,$2E,$2F
	.byte	$73,$1E,$8D,$78,$68,$93,$4B,$A1,$31,$CF,$8F,$6A,$47,$D5,$AA,$2A
	.byte	$CB,$75,$D8,$CE,$1D,$11,$F9,$00,$74,$DD,$44,$7F,$9B,$CC,$AA,$30
	.byte	$5B,$40,$31,$9B,$A4,$7B,$BF,$CD,$D1,$3E,$E4,$4E,$EA,$C4,$61,$27
	.byte	$05,$8B,$15,$E8,$FE,$FD,$18,$D4,$80

qr049:	.byte	33
	.byte	$FE,$22,$33,$3F,$C1,$22,$3B,$D0,$6E,$A4,$46,$0B,$B7,$49,$8D,$45
	.byte	$DB,$A4,$CD,$12,$EC,$11,$D9,$CD,$07,$FA,$AA,$AA,$FE,$01,$22,$EE
	.byte	$00,$EF,$88,$8C,$E2,$48,$2E,$66,$53,$57,$AE,$88,$0A,$3C,$9D,$D3
	.byte	$DE,$A7,$2B,$27,$CF,$0E,$C1,$9B,$77,$16,$AA,$93,$B9,$B7,$BB,$A1
	.byte	$E9,$3A,$93,$E8,$1D,$F1,$B4,$CE,$77,$11,$76,$F6,$9B,$2F,$67,$39
	.byte	$DF,$B4,$20,$2B,$2A,$46,$1B,$85,$96,$65,$26,$BE,$97,$80,$FE,$D8
	.byte	$60,$EF,$72,$BF,$48,$AC,$FC,$00,$5E,$27,$C6,$FF,$A6,$99,$EB,$70
	.byte	$5D,$DF,$91,$0B,$A9,$22,$EF,$95,$D0,$9A,$4E,$EE,$EB,$91,$B4,$73
	.byte	$05,$21,$3F,$42,$FE,$E8,$CD,$81,$80

qr050:	.byte	33
	.byte	$FE,$EA,$21,$3F,$C1,$70,$0D,$10,$6E,$AD,$1B,$6B,$B7,$5A,$65,$05
	.byte	$DB,$A0,$78,$32,$EC,$14,$F7,$A1,$07,$FA,$AA,$AA,$FE,$00,$97,$8A
	.byte	$00,$CE,$16,$1E,$97,$C0,$F4,$E0,$88,$0B,$E7,$CB,$67,$8A,$06,$8F
	.byte	$97,$86,$3A,$CD,$6B,$9E,$06,$3D,$1A,$A2,$FA,$87,$AF,$6D,$E2,$54
	.byte	$BD,$A8,$22,$97,$47,$B8,$AA,$F4,$99,$CA,$39,$A7,$F0,$42,$C5,$86
	.byte	$9C,$FD,$0A,$BA,$CD,$62,$8E,$44,$3F,$08,$90,$79,$04,$B6,$24,$10
	.byte	$97,$9B,$E0,$DA,$D6,$DE,$FD,$00,$64,$D1,$45,$BF,$97,$EA,$AA,$D0
	.byte	$56,$8D,$D1,$3B,$AC,$D5,$CF,$95,$D0,$39,$23,$52,$E9,$07,$82,$A9
	.byte	$05,$95,$DB,$D0,$FE,$B6,$DF,$C8,$80

qr051:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$58,$50,$6E,$84,$EA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$56,$09,$81,$FC,$64,$88,$D0,$61,$B2,$24,$30,$2C,$76,$7D,$06,$72
	.byte	$4A,$59,$D7,$1E,$BE,$BE,$90,$0A,$32,$82,$F6,$BF,$48,$27,$45,$C7
	.byte	$DF,$65,$AA,$C3,$59,$12,$71,$29,$29,$B8,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$4D,$43,$1B,$BA,$C7,$CF,$B5,$D4,$94,$80,$6E,$A3
	.byte	$7C,$F5,$05,$A3,$1A,$AF,$E9,$D5,$6E,$00

qr052:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$07,$10,$6E,$9C,$A7,$8B,$B7,$5A,$1B,$75
	.byte	$DB,$A2,$F6,$F2,$EC,$15,$3D,$01,$07,$FA,$AA,$AA,$FE,$00,$1A,$1D
	.byte	$00,$FB,$EB,$42,$D5,$74,$20,$FE,$B0,$D9,$B6,$11,$84,$13,$BE,$57
	.byte	$E6,$42,$3D,$0D,$2C,$82,$6B,$79,$F9,$2D,$0C,$1E,$21,$55,$61,$9A
	.byte	$3A,$B4,$32,$2B,$73,$C9,$18,$70,$FF,$F2,$D2,$86,$12,$A1,$5D,$96
	.byte	$55,$8C,$CB,$3B,$05,$25,$95,$2A,$78,$EB,$1E,$7E,$1C,$B8,$1D,$5A
	.byte	$59,$0C,$FC,$A7,$6B,$C2,$FC,$80,$70,$4F,$45,$7F,$AE,$7A,$6B,$50
	.byte	$4E,$64,$B1,$DB,$AD,$17,$0F,$95,$D5,$77,$C0,$DE,$EA,$1F,$0C,$91
	.byte	$05,$99,$5C,$CC,$FE,$8B,$C3,$B9,$00

qr053:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$C8,$50,$6E,$9C,$EA,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E2,$55
	.byte	$56,$62,$B1,$FC,$43,$D1,$D0,$61,$63,$D4,$30,$2A,$F4,$DD,$06,$2A
	.byte	$B8,$59,$D7,$C9,$1E,$BE,$96,$96,$72,$82,$DF,$67,$48,$25,$0B,$87
	.byte	$DF,$64,$E2,$C3,$59,$78,$E5,$29,$2A,$21,$FC,$FB,$80,$6E,$5C,$7F
	.byte	$FA,$AC,$6B,$90,$47,$43,$18,$BA,$A5,$CF,$B5,$D4,$A4,$80,$6E,$A1
	.byte	$FC,$F5,$05,$93,$1A,$AF,$EE,$75,$6E,$00

qr054:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$C8,$50,$6E,$9C,$EA,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E2,$55
	.byte	$51,$02,$B1,$FC,$7A,$91,$D0,$61,$F1,$D4,$30,$29,$EC,$FD,$06,$3E
	.byte	$18,$59,$D5,$19,$8E,$BE,$8A,$96,$32,$82,$56,$47,$48,$24,$4B,$97
	.byte	$DF,$6A,$AA,$C3,$59,$2B,$25,$29,$2A,$E7,$FC,$FB,$80,$7C,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$43,$43,$18,$BA,$E5,$CF,$B5,$D6,$A4,$80,$6E,$B1
	.byte	$FC,$F5,$05,$53,$1A,$AF,$EA,$75,$6E,$00

qr055:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$08,$50,$6E,$92,$EA,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$53,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$55,$E2,$A1,$FC,$70,$93,$D0,$61,$28,$E4,$30,$21,$E5,$BD,$06,$26
	.byte	$BF,$59,$D7,$7C,$3E,$BE,$83,$2F,$72,$82,$4F,$A7,$48,$26,$61,$87
	.byte	$DF,$66,$B8,$C3,$59,$68,$55,$29,$29,$F8,$7C,$FB,$80,$48,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$42,$43,$19,$BA,$BF,$CF,$B5,$D4,$C4,$80,$6E,$B1
	.byte	$FC,$F5,$05,$F7,$1A,$AF,$E8,$75,$6E,$00

qr056:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$78,$50,$6E,$84,$EA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$55,$01,$81,$FC,$42,$E9,$D0,$61,$8A,$20,$30,$29,$FE,$7D,$06,$74
	.byte	$CA,$59,$D4,$EB,$A6,$BE,$89,$02,$72,$82,$0B,$FF,$48,$27,$06,$C7
	.byte	$DF,$63,$A3,$C3,$59,$41,$75,$29,$28,$E8,$7C,$FB,$80,$7A,$5C,$7F
	.byte	$FA,$84,$6B,$90,$45,$03,$1B,$BA,$87,$CF,$B5,$D4,$94,$80,$6E,$B2
	.byte	$FC,$F5,$05,$63,$1A,$AF,$E9,$D5,$6E,$00

qr057:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$28,$50,$6E,$9C,$6A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$55,$28,$D1,$FC,$50,$EB,$D0,$61,$B9,$34,$30,$2C,$26,$3D,$06,$18
	.byte	$2B,$59,$D4,$5B,$9E,$BE,$91,$31,$72,$82,$03,$47,$48,$24,$0F,$97
	.byte	$DF,$66,$D2,$43,$59,$49,$A1,$29,$2A,$FB,$FC,$FB,$80,$7E,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$47,$43,$1A,$BA,$C7,$CF,$A5,$D6,$A4,$80,$6E,$A1
	.byte	$7C,$F5,$05,$47,$1A,$AF,$EA,$F5,$6E,$00

qr058:	.byte	29
	.byte	$FE,$2A,$9B,$FC,$15,$28,$50,$6E,$82,$6A,$BB,$75,$DE,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$51,$6D,$61,$FC,$7F,$EA,$D0,$61,$71,$20,$30,$2C,$6E,$7D,$06,$78
	.byte	$68,$59,$D4,$2C,$16,$BE,$88,$15,$32,$82,$5E,$CF,$48,$24,$80,$97
	.byte	$DF,$66,$89,$43,$59,$6B,$65,$29,$29,$EC,$5C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$4A,$03,$1A,$BA,$BF,$CF,$B5,$D7,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EC,$75,$6E,$00

qr059:	.byte	33
	.byte	$FE,$F7,$46,$3F,$C1,$0F,$89,$10,$6E,$95,$8F,$0B,$B7,$46,$69,$B5
	.byte	$DB,$A3,$D3,$62,$EC,$12,$B2,$79,$07,$FA,$AA,$AA,$FE,$01,$94,$65
	.byte	$00,$DA,$4F,$D0,$A0,$C2,$51,$19,$AC,$88,$D2,$93,$CD,$3F,$00,$6A
	.byte	$05,$C0,$EE,$61,$9A,$5D,$EB,$40,$1A,$A0,$FB,$0D,$E8,$71,$03,$5D
	.byte	$26,$C5,$32,$F9,$32,$ED,$EE,$B8,$14,$7C,$E1,$BB,$A4,$7A,$34,$B6
	.byte	$B4,$02,$FF,$33,$4D,$01,$06,$26,$64,$9A,$DA,$0A,$55,$BC,$8F,$5A
	.byte	$FA,$B2,$C4,$EF,$1D,$09,$F9,$00,$63,$C1,$45,$BF,$97,$5E,$EB,$10
	.byte	$42,$14,$71,$CB,$AC,$32,$1F,$DD,$D7,$F9,$F8,$3E,$E8,$C4,$61,$27
	.byte	$05,$17,$64,$2F,$FE,$CF,$51,$F0,$00

qr060:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$0C,$50,$6E,$9C,$EA,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$53,$C4,$E1,$FC,$77,$8A,$D0,$60,$59,$30,$30,$2C,$26,$7D,$06,$36
	.byte	$28,$59,$D5,$28,$8E,$BE,$88,$A9,$72,$82,$EF,$C3,$48,$26,$6C,$B7
	.byte	$DF,$63,$E3,$C3,$59,$63,$E5,$29,$2B,$3F,$FC,$FB,$80,$4C,$5C,$7F
	.byte	$FA,$0C,$6B,$90,$4F,$43,$1A,$BA,$C7,$CF,$A5,$D7,$84,$80,$6E,$A1
	.byte	$FC,$F5,$05,$67,$1A,$AF,$EE,$F5,$6E,$00

qr061:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$1C,$50,$6E,$9C,$6A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$54,$8C,$E1,$FC,$4E,$EA,$D0,$60,$63,$34,$30,$26,$7E,$7D,$06,$5C
	.byte	$0A,$59,$D5,$39,$06,$BE,$95,$39,$72,$82,$82,$27,$48,$25,$85,$A7
	.byte	$DF,$68,$FB,$C3,$59,$01,$E1,$29,$29,$65,$FC,$FB,$80,$7C,$5C,$7F
	.byte	$FA,$24,$6B,$90,$4B,$43,$1A,$BA,$87,$CF,$A5,$D7,$94,$80,$6E,$A8
	.byte	$FC,$F5,$05,$63,$1A,$AF,$EB,$F5,$6E,$00

qr062:	.byte	33
	.byte	$FE,$41,$BD,$3F,$C1,$6C,$13,$10,$6E,$9C,$A7,$8B,$B7,$5A,$1A,$75
	.byte	$DB,$A2,$F1,$F2,$EC,$15,$3C,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$4D
	.byte	$00,$FB,$EB,$52,$D5,$00,$20,$FE,$B0,$C4,$B6,$13,$84,$03,$9E,$51
	.byte	$E6,$46,$75,$0D,$2C,$80,$4D,$78,$F9,$2C,$EC,$9F,$81,$55,$AA,$DA
	.byte	$6A,$B4,$7E,$0B,$6B,$C9,$0A,$B0,$FF,$F2,$D0,$AE,$12,$A1,$51,$02
	.byte	$55,$8C,$C3,$F9,$04,$25,$96,$88,$78,$EB,$1E,$08,$9C,$98,$1D,$4A
	.byte	$59,$1C,$FC,$9A,$2B,$CA,$FC,$80,$40,$4F,$45,$7F,$AE,$78,$6B,$50
	.byte	$4E,$64,$B1,$DB,$AF,$17,$8F,$95,$D4,$77,$C0,$DE,$EB,$9F,$2C,$91
	.byte	$05,$59,$5C,$CC,$FE,$8B,$C3,$B9,$00

qr063:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$C8,$50,$6E,$9C,$AA,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$57,$6A,$81,$FC,$42,$D1,$50,$60,$92,$D4,$30,$20,$24,$FD,$06,$40
	.byte	$3A,$59,$D6,$9E,$1E,$BE,$9E,$B6,$72,$82,$5E,$E7,$48,$25,$41,$A7
	.byte	$DF,$6E,$DB,$C3,$59,$1B,$A1,$29,$28,$23,$FC,$FB,$80,$6D,$5C,$7F
	.byte	$FB,$B4,$6B,$90,$4B,$43,$18,$BA,$A5,$CF,$B5,$D7,$A4,$80,$6E,$A1
	.byte	$FC,$F5,$05,$53,$1A,$AF,$EE,$75,$6E,$00

qr064:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$C3,$10,$6E,$83,$45,$8B,$B7,$5D,$68,$75
	.byte	$DB,$A4,$1F,$F2,$EC,$16,$C2,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$7D
	.byte	$00,$FB,$D0,$DA,$D5,$3A,$97,$96,$B0,$C4,$A9,$E1,$84,$13,$21,$A6
	.byte	$E6,$4C,$26,$B4,$AC,$84,$04,$0D,$F9,$2E,$3C,$60,$21,$54,$F9,$E5
	.byte	$1A,$B4,$82,$B0,$43,$C9,$24,$77,$8F,$F2,$F4,$A1,$D8,$A1,$4E,$B5
	.byte	$95,$8C,$C6,$34,$BE,$25,$97,$C2,$07,$EB,$1E,$69,$67,$18,$1D,$62
	.byte	$A6,$7C,$FC,$B7,$51,$52,$FC,$80,$67,$4F,$45,$7F,$A9,$EA,$6B,$50
	.byte	$45,$91,$B1,$FB,$AC,$BC,$0F,$95,$D7,$04,$C0,$DE,$EB,$E5,$8C,$91
	.byte	$05,$65,$7C,$CC,$FE,$F0,$63,$B9,$00

qr065:	.byte	33
	.byte	$FE,$F7,$16,$3F,$C1,$48,$B1,$50,$6E,$91,$15,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9A,$42,$EC,$17,$74,$25,$07,$FA,$AA,$AA,$FE,$01,$88,$74
	.byte	$00,$E6,$DD,$89,$F9,$92,$C4,$4C,$F9,$DA,$BB,$AD,$5F,$66,$87,$74
	.byte	$74,$08,$66,$61,$9A,$5E,$C1,$30,$DD,$BE,$48,$C5,$4C,$E3,$C3,$48
	.byte	$73,$90,$A6,$7D,$A0,$A4,$D6,$64,$65,$BB,$FD,$83,$A4,$7A,$39,$0F
	.byte	$71,$1E,$8D,$24,$69,$93,$49,$E3,$31,$CF,$8F,$BA,$C7,$F5,$AA,$68
	.byte	$0B,$65,$D8,$D7,$3D,$11,$F9,$00,$74,$DD,$44,$7F,$9B,$CC,$AA,$30
	.byte	$53,$40,$31,$9B,$A6,$7A,$BF,$CD,$D3,$3E,$E4,$4E,$EA,$C4,$61,$27
	.byte	$05,$8B,$35,$E8,$FE,$FD,$18,$D4,$80

qr066:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$48,$50,$6E,$84,$AA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$62,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$50,$4D,$81,$FC,$40,$88,$D0,$60,$10,$24,$30,$21,$26,$7D,$06,$6E
	.byte	$48,$59,$D7,$EC,$B6,$BE,$91,$96,$32,$82,$FE,$DF,$48,$24,$2E,$E7
	.byte	$DF,$65,$B2,$43,$59,$4A,$75,$29,$28,$AE,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FB,$14,$6B,$90,$4D,$03,$1B,$BA,$A7,$CF,$B5,$D6,$84,$80,$6E,$BA
	.byte	$FC,$F5,$05,$63,$1A,$AF,$EB,$D5,$6E,$00

qr067:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$18,$50,$6E,$9C,$2A,$BB,$75,$4E,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$55,$A8,$D1,$FC,$77,$CA,$50,$60,$53,$30,$30,$29,$3E,$3D,$06,$4A
	.byte	$AB,$59,$D7,$18,$1E,$BE,$8A,$81,$72,$82,$6A,$E7,$48,$24,$84,$97
	.byte	$DF,$6A,$83,$43,$59,$03,$E5,$29,$2B,$A7,$FC,$FB,$80,$4E,$5C,$7F
	.byte	$FB,$0C,$6B,$90,$4F,$43,$1A,$BA,$DF,$CF,$A5,$D6,$84,$80,$6E,$A8
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EE,$F5,$6E,$00

qr068:	.byte	33
	.byte	$FE,$42,$BD,$3F,$C1,$6F,$F7,$10,$6E,$9C,$47,$8B,$B7,$5A,$2B,$75
	.byte	$DB,$A2,$CE,$F2,$EC,$15,$3E,$01,$07,$FA,$AA,$AA,$FE,$00,$19,$2D
	.byte	$00,$FB,$EB,$CA,$D5,$3E,$A0,$7E,$B0,$C6,$96,$43,$84,$02,$8E,$55
	.byte	$E6,$4F,$65,$04,$2C,$84,$09,$73,$F9,$2F,$EB,$9D,$21,$54,$19,$58
	.byte	$2A,$B4,$D2,$6B,$D3,$C9,$6E,$90,$4F,$F2,$CA,$DE,$7A,$A1,$47,$86
	.byte	$66,$8C,$C2,$35,$18,$25,$94,$A0,$76,$EB,$1E,$39,$1D,$38,$1D,$19
	.byte	$D9,$0C,$FC,$B2,$AA,$52,$FC,$80,$40,$0F,$45,$7F,$AE,$40,$6B,$50
	.byte	$46,$75,$B1,$CB,$AD,$1D,$0F,$9D,$D4,$75,$C0,$DE,$EB,$9E,$0C,$91
	.byte	$05,$58,$3C,$CC,$FE,$AB,$C3,$B9,$00

qr069:	.byte	33
	.byte	$FE,$22,$73,$3F,$C1,$70,$66,$D0,$6E,$A0,$D4,$4B,$B7,$49,$87,$45
	.byte	$DB,$AE,$86,$32,$EC,$15,$49,$85,$07,$FA,$AA,$AA,$FE,$01,$68,$FA
	.byte	$00,$D3,$1A,$1D,$BB,$4E,$6E,$7E,$53,$78,$87,$A6,$98,$6D,$2F,$A6
	.byte	$FA,$30,$BB,$2C,$4F,$0E,$C4,$B0,$E5,$5D,$9E,$59,$1D,$24,$DA,$61
	.byte	$A9,$3A,$56,$2C,$CF,$B8,$B6,$77,$43,$83,$0B,$EE,$A9,$2F,$73,$AD
	.byte	$4F,$FD,$06,$7A,$18,$54,$52,$88,$93,$65,$27,$28,$00,$49,$DA,$B8
	.byte	$C4,$3D,$3B,$82,$C8,$5C,$FC,$00,$6C,$0E,$C6,$7F,$A2,$01,$AA,$50
	.byte	$49,$EF,$91,$3B,$A5,$6C,$CF,$95,$D6,$05,$07,$C2,$E9,$13,$B4,$73
	.byte	$05,$28,$BB,$D0,$FE,$DB,$A4,$A5,$00

qr070:	.byte	33
	.byte	$FE,$4C,$9B,$3F,$C1,$4F,$A6,$D0,$6E,$BF,$36,$4B,$B7,$4E,$ED,$45
	.byte	$DB,$A8,$6D,$32,$EC,$16,$B3,$85,$07,$FA,$AA,$AA,$FE,$01,$94,$CA
	.byte	$00,$D3,$21,$85,$BB,$4C,$99,$B6,$53,$4C,$98,$1E,$98,$7A,$10,$52
	.byte	$FA,$36,$70,$85,$CF,$0C,$CB,$C2,$E5,$5F,$5C,$A1,$BD,$25,$23,$9E
	.byte	$F9,$3A,$1A,$16,$57,$B8,$D0,$30,$43,$83,$3C,$A9,$19,$2F,$6A,$3A
	.byte	$8D,$FD,$0D,$A9,$A1,$54,$51,$28,$E6,$65,$27,$FD,$F8,$69,$DA,$CA
	.byte	$79,$5D,$3B,$97,$33,$C4,$FC,$00,$5B,$7E,$C6,$7F,$BD,$AB,$AA,$50
	.byte	$4E,$09,$91,$0B,$A4,$CE,$CF,$95,$D6,$76,$07,$C2,$E8,$69,$34,$73
	.byte	$05,$17,$BB,$D0,$FE,$C0,$04,$A5,$00

qr071:	.byte	29
	.byte	$FE,$21,$13,$FC,$12,$0B,$D0,$6E,$A4,$C8,$BB,$74,$86,$25,$DB,$A4
	.byte	$B7,$2E,$C1,$1A,$D9,$07,$FA,$AA,$AF,$E0,$11,$DD,$00,$EF,$85,$DE
	.byte	$27,$8E,$52,$72,$53,$E9,$B3,$EE,$70,$E8,$D3,$A8,$62,$C5,$E5,$94
	.byte	$F5,$61,$37,$5F,$98,$86,$73,$0D,$BC,$BA,$D3,$A4,$C6,$19,$21,$44
	.byte	$51,$67,$F2,$20,$D6,$E2,$19,$CA,$AB,$7F,$C4,$F8,$00,$42,$64,$5F
	.byte	$FA,$22,$6B,$70,$58,$CD,$10,$BA,$86,$4F,$8D,$D1,$47,$0E,$6E,$A1
	.byte	$1F,$7B,$05,$EB,$F9,$2F,$E8,$4D,$8D,$80

qr072:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$28,$50,$6E,$92,$2A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$0F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$56,$E6,$81,$FC,$71,$B2,$D0,$60,$E0,$E4,$30,$2C,$35,$BD,$06,$36
	.byte	$BF,$59,$D4,$6E,$BE,$BE,$81,$9B,$72,$82,$FB,$A7,$48,$25,$CE,$87
	.byte	$DF,$6C,$C1,$C3,$59,$4B,$51,$29,$2B,$2E,$7C,$FB,$80,$58,$5C,$7F
	.byte	$FA,$24,$6B,$90,$4E,$43,$19,$BA,$FF,$CF,$B5,$D5,$E4,$80,$6E,$B2
	.byte	$FC,$F5,$05,$83,$1A,$AF,$E9,$F5,$6E,$00

qr073:	.byte	29
	.byte	$FE,$2A,$9B,$FC,$15,$28,$50,$6E,$82,$2A,$BB,$75,$DE,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$57,$C5,$61,$FC,$64,$8B,$D0,$61,$B0,$20,$30,$27,$B6,$7D,$06,$44
	.byte	$AB,$59,$D4,$1B,$16,$BE,$9C,$8D,$32,$82,$3F,$6F,$48,$25,$2F,$B7
	.byte	$DF,$62,$E0,$43,$59,$10,$25,$29,$29,$64,$5C,$FB,$80,$4A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$4E,$03,$1A,$BA,$DF,$CF,$B5,$D4,$D4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EA,$75,$6E,$00

qr074:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$E8,$50,$6E,$9C,$2A,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$50,$6A,$81,$FC,$43,$91,$D0,$61,$CA,$D4,$30,$2E,$74,$FD,$06,$38
	.byte	$F8,$59,$D6,$3E,$1E,$BE,$90,$16,$72,$82,$0A,$47,$48,$24,$A9,$87
	.byte	$DF,$6C,$9A,$C3,$59,$5A,$A5,$29,$29,$37,$FC,$FB,$80,$4D,$5C,$7F
	.byte	$FA,$B4,$6B,$90,$47,$03,$18,$BA,$85,$CF,$B5,$D4,$A4,$80,$6E,$A1
	.byte	$FC,$F5,$05,$53,$1A,$AF,$E8,$75,$6E,$00

qr075:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$28,$50,$6E,$92,$2A,$BB,$75,$9E,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$53,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$53,$8A,$A1,$FC,$40,$93,$D0,$60,$42,$E0,$30,$2F,$BD,$FD,$06,$3A
	.byte	$7D,$59,$D7,$2E,$2E,$BE,$8D,$0B,$32,$82,$7B,$67,$48,$27,$C0,$A7
	.byte	$DF,$6C,$F8,$C3,$59,$28,$51,$29,$29,$2C,$7C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$24,$6B,$90,$4A,$43,$19,$BA,$FF,$CF,$B5,$D4,$E4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$83,$1A,$AF,$EA,$F5,$6E,$00

qr076:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$3C,$50,$6E,$82,$AA,$BB,$75,$DA,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$D6,$55
	.byte	$55,$C5,$51,$FC,$6E,$88,$50,$61,$F9,$14,$30,$26,$BE,$FD,$06,$2C
	.byte	$81,$59,$D5,$1D,$F6,$BE,$9C,$A6,$32,$82,$0F,$B5,$48,$25,$21,$67
	.byte	$DF,$65,$C1,$C3,$59,$78,$51,$29,$28,$7A,$FC,$FB,$80,$71,$5C,$7F
	.byte	$FA,$7C,$6B,$90,$42,$43,$1A,$BA,$BF,$CF,$AD,$D7,$44,$80,$EE,$AA
	.byte	$7C,$F5,$05,$27,$1A,$AF,$EA,$75,$6E,$00

qr077:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$CC,$50,$6E,$9C,$AA,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$53,$0E,$81,$FC,$45,$B1,$D0,$60,$D3,$D4,$30,$27,$AC,$FD,$06,$74
	.byte	$38,$59,$D6,$18,$0E,$BE,$9A,$96,$72,$82,$07,$C7,$48,$25,$E5,$97
	.byte	$DF,$60,$B2,$C3,$59,$48,$21,$29,$2B,$BF,$FC,$FB,$80,$7F,$5C,$7F
	.byte	$FB,$B4,$6B,$90,$47,$03,$18,$BA,$C5,$CF,$B5,$D5,$A4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$53,$1A,$AF,$EC,$75,$6E,$00

qr078:	.byte	33
	.byte	$FE,$F7,$16,$3F,$C1,$0F,$AD,$10,$6E,$95,$83,$0B,$B7,$46,$6B,$B5
	.byte	$DB,$A3,$D3,$62,$EC,$12,$B1,$79,$07,$FA,$AA,$AA,$FE,$01,$94,$35
	.byte	$00,$DA,$4F,$D0,$A0,$82,$11,$19,$AC,$A6,$B2,$91,$CD,$21,$00,$68
	.byte	$05,$C4,$BE,$60,$9A,$58,$05,$42,$1A,$A3,$2F,$8C,$C8,$70,$01,$5D
	.byte	$16,$C5,$26,$19,$32,$ED,$C8,$78,$1C,$7C,$FE,$C3,$A4,$7A,$3F,$36
	.byte	$B7,$02,$FF,$AB,$4D,$01,$05,$6E,$64,$9A,$DA,$DA,$D5,$BC,$8F,$39
	.byte	$7A,$92,$C4,$E2,$DD,$09,$F9,$00,$53,$C1,$45,$BF,$9F,$5E,$EB,$10
	.byte	$4A,$15,$71,$CB,$AA,$32,$9F,$DD,$D7,$F9,$F8,$3E,$E8,$C4,$41,$27
	.byte	$05,$57,$44,$2F,$FE,$CF,$51,$F0,$00

qr079:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$4C,$50,$6E,$84,$AA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$4F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$56,$89,$81,$FC,$69,$C8,$50,$60,$C3,$24,$30,$29,$66,$7D,$06,$4C
	.byte	$8B,$59,$D5,$7E,$A6,$BE,$92,$A6,$32,$82,$1E,$1F,$48,$26,$81,$E7
	.byte	$DF,$6E,$FA,$43,$59,$11,$F5,$29,$28,$E2,$7C,$FB,$80,$58,$5C,$7F
	.byte	$FB,$04,$6B,$90,$4D,$43,$1B,$BA,$A7,$CF,$B5,$D7,$A4,$80,$6E,$BB
	.byte	$FC,$F5,$05,$A3,$1A,$AF,$EB,$D5,$6E,$00

qr080:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$4C,$50,$6E,$9C,$AA,$BB,$75,$7E,$C5,$DB,$A5
	.byte	$CF,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$13,$00,$FB,$A6,$55
	.byte	$53,$87,$81,$FC,$5E,$88,$50,$60,$90,$94,$30,$28,$E4,$7D,$06,$28
	.byte	$AB,$59,$D7,$A9,$A6,$BE,$9D,$2A,$32,$82,$CF,$3F,$48,$24,$6F,$F7
	.byte	$DF,$67,$B0,$43,$59,$10,$75,$29,$2B,$3A,$3C,$FB,$80,$7B,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4F,$43,$18,$BA,$E7,$CF,$A5,$D6,$94,$80,$6E,$B9
	.byte	$FC,$F5,$05,$57,$1A,$AF,$EC,$F5,$6E,$00

qr081:	.byte	29
	.byte	$FE,$23,$13,$FC,$17,$1B,$50,$6E,$A0,$DA,$BB,$74,$86,$25,$DB,$AE
	.byte	$BE,$2E,$C1,$50,$FD,$07,$FA,$AA,$AF,$E0,$15,$0F,$00,$D3,$17,$93
	.byte	$B1,$0E,$62,$72,$75,$A1,$17,$7D,$12,$C8,$41,$E5,$E2,$C5,$E5,$C6
	.byte	$65,$28,$12,$3D,$D1,$A2,$FE,$21,$FC,$BA,$A6,$C0,$54,$53,$8B,$D6
	.byte	$18,$68,$D2,$A0,$D6,$7B,$14,$EE,$38,$7A,$E0,$FA,$00,$53,$64,$5F
	.byte	$FB,$A8,$2A,$50,$42,$85,$14,$BA,$06,$4F,$8D,$D4,$63,$9C,$2E,$85
	.byte	$8D,$33,$05,$AB,$F9,$2F,$EA,$04,$A9,$00

qr082:	.byte	29
	.byte	$FE,$C2,$43,$FC,$13,$04,$90,$6E,$9B,$8E,$BB,$74,$59,$D5,$DB,$A0
	.byte	$6B,$2E,$C1,$29,$01,$07,$FA,$AA,$AF,$E0,$1A,$F0,$00,$DA,$42,$C2
	.byte	$0B,$05,$9D,$8D,$9A,$96,$42,$28,$83,$03,$BE,$1A,$76,$D0,$B0,$DA
	.byte	$9C,$D7,$ED,$2B,$2C,$F7,$B4,$8B,$03,$45,$3B,$75,$01,$05,$86,$19
	.byte	$E7,$BB,$9C,$F5,$83,$A8,$9F,$11,$CF,$3E,$35,$FF,$00,$48,$9C,$63
	.byte	$F8,$65,$6B,$10,$44,$7B,$1A,$BA,$89,$1F,$DD,$D7,$EC,$63,$EE,$92
	.byte	$58,$67,$05,$50,$06,$DF,$E8,$D1,$FC,$00

qr083:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$EC,$50,$6E,$9C,$2A,$BB,$75,$BE,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$50,$C2,$91,$FC,$63,$D0,$50,$61,$AB,$D0,$30,$29,$E4,$FD,$06,$14
	.byte	$D9,$59,$D5,$E9,$06,$BE,$86,$B6,$32,$82,$2F,$47,$48,$27,$41,$A7
	.byte	$DF,$65,$C2,$C3,$59,$08,$A1,$29,$28,$BD,$FC,$FB,$80,$7C,$5C,$7F
	.byte	$FB,$AC,$6B,$90,$4B,$43,$18,$BA,$C5,$CF,$B5,$D5,$A4,$80,$6E,$A9
	.byte	$FC,$F5,$05,$13,$1A,$AF,$E8,$75,$6E,$00

qr084:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$5C,$50,$6E,$84,$AA,$BB,$75,$CE,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$60,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$57,$0D,$91,$FC,$51,$C9,$50,$61,$13,$20,$30,$22,$AE,$7D,$06,$5A
	.byte	$C8,$59,$D6,$3F,$AE,$BE,$91,$06,$32,$82,$D7,$9F,$48,$25,$A8,$E7
	.byte	$DF,$60,$FA,$43,$59,$50,$B5,$29,$28,$3A,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FB,$04,$6B,$90,$41,$43,$1B,$BA,$A7,$CF,$B5,$D5,$A4,$80,$6E,$B3
	.byte	$FC,$F5,$05,$A3,$1A,$AF,$EF,$D5,$6E,$00

qr085:	.byte	33
	.byte	$FE,$E8,$51,$3F,$C1,$70,$6D,$10,$6E,$AD,$7B,$6B,$B7,$5A,$75,$05
	.byte	$DB,$A0,$6E,$32,$EC,$14,$F8,$A1,$07,$FA,$AA,$AA,$FE,$00,$94,$CA
	.byte	$00,$CE,$17,$DE,$97,$C8,$F4,$10,$88,$3A,$87,$D1,$67,$91,$A6,$BC
	.byte	$97,$86,$32,$C4,$EB,$9F,$80,$35,$1A,$A0,$7B,$02,$2F,$6D,$52,$D4
	.byte	$FD,$A8,$5A,$37,$47,$B8,$9C,$34,$39,$CA,$28,$FF,$E0,$42,$DA,$BA
	.byte	$8D,$FD,$0E,$B2,$CF,$E2,$8F,$05,$3F,$08,$90,$5F,$04,$96,$24,$59
	.byte	$97,$AB,$E0,$C2,$B6,$56,$FD,$00,$54,$C1,$45,$BF,$87,$D2,$AA,$D0
	.byte	$52,$8C,$D1,$3B,$A8,$CD,$CF,$8D,$D2,$3C,$23,$52,$E9,$86,$02,$A9
	.byte	$05,$54,$8B,$D0,$FE,$B7,$6F,$C8,$80

qr086:	.byte	29
	.byte	$FE,$23,$13,$FC,$12,$2B,$D0,$6E,$A4,$28,$BB,$74,$84,$25,$DB,$A4
	.byte	$97,$2E,$C1,$18,$D9,$07,$FA,$AA,$AF,$E0,$11,$9D,$00,$EF,$85,$DE
	.byte	$23,$42,$62,$72,$6D,$C8,$33,$EF,$D1,$E8,$D3,$A5,$3A,$C5,$E5,$D4
	.byte	$14,$61,$37,$49,$88,$86,$79,$1D,$BC,$BA,$97,$84,$C6,$19,$24,$64
	.byte	$51,$63,$BA,$A0,$D6,$83,$1D,$CA,$AA,$F9,$C4,$F8,$00,$53,$64,$5F
	.byte	$FB,$A2,$6B,$70,$50,$8D,$10,$BA,$A6,$4F,$8D,$D1,$47,$0E,$6E,$B9
	.byte	$1F,$7B,$05,$6B,$F9,$2F,$EE,$4D,$8D,$80

qr087:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$D8,$50,$6E,$9C,$CA,$BB,$75,$BC,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$52,$CE,$81,$FC,$77,$F0,$D0,$60,$FA,$D0,$30,$22,$2C,$BD,$06,$34
	.byte	$FB,$59,$D4,$BD,$9E,$BE,$83,$3E,$72,$82,$AA,$67,$48,$24,$45,$A7
	.byte	$DF,$68,$A3,$C3,$59,$4B,$25,$29,$28,$21,$FC,$FB,$80,$4E,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$4F,$03,$18,$BA,$85,$CF,$B5,$D4,$A4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$13,$1A,$AF,$EE,$75,$6E,$00

qr088:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$18,$50,$6E,$9C,$CA,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$57,$4C,$E1,$FC,$77,$EA,$D0,$61,$D3,$30,$30,$2F,$26,$3D,$06,$22
	.byte	$CB,$59,$D5,$1B,$1E,$BE,$8E,$B1,$72,$82,$4A,$27,$48,$26,$4A,$97
	.byte	$DF,$67,$DB,$43,$59,$33,$A5,$29,$28,$AF,$FC,$FB,$80,$4D,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$4C,$03,$1A,$BA,$BF,$CF,$A5,$D5,$A4,$80,$6E,$BB
	.byte	$FC,$F5,$05,$37,$1A,$AF,$EA,$75,$6E,$00

qr089:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$18,$50,$6E,$82,$CA,$BB,$75,$DC,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6E,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$D6,$55
	.byte	$55,$41,$61,$FC,$6A,$A9,$D0,$61,$7A,$14,$30,$21,$26,$DD,$06,$74
	.byte	$82,$59,$D7,$E9,$7E,$BE,$9C,$82,$32,$82,$73,$9F,$48,$24,$E9,$57
	.byte	$DF,$6B,$82,$43,$59,$4A,$25,$29,$2B,$FE,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FA,$94,$6B,$90,$4D,$03,$1B,$BA,$AD,$CF,$AD,$D5,$A4,$80,$EE,$A9
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$EC,$75,$6E,$00

qr090:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$48,$50,$6E,$84,$CA,$BB,$75,$CC,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$52,$C1,$A1,$FC,$7A,$88,$50,$61,$A3,$20,$30,$2F,$6E,$7D,$06,$60
	.byte	$49,$59,$D7,$DA,$36,$BE,$86,$9E,$32,$82,$B2,$1F,$48,$27,$4D,$E7
	.byte	$DF,$62,$8A,$43,$59,$12,$75,$29,$28,$FE,$7C,$FB,$80,$69,$5C,$7F
	.byte	$FB,$14,$6B,$90,$41,$43,$1B,$BA,$C7,$CF,$B5,$D6,$84,$80,$6E,$A2
	.byte	$FC,$F5,$05,$E7,$1A,$AF,$E9,$D5,$6E,$00

qr091:	.byte	33
	.byte	$FE,$77,$46,$3F,$C1,$0C,$95,$10,$6E,$9C,$AB,$8B,$B7,$56,$69,$B5
	.byte	$DB,$AA,$F3,$F2,$EC,$13,$34,$61,$07,$FA,$AA,$AA,$FE,$00,$98,$15
	.byte	$00,$C7,$6B,$42,$8C,$5E,$51,$09,$AC,$BA,$B6,$1B,$84,$1E,$26,$76
	.byte	$64,$4E,$FE,$60,$9A,$5B,$4B,$70,$D9,$AF,$CA,$9F,$A1,$55,$1A,$DD
	.byte	$26,$C5,$F3,$2B,$7B,$C9,$46,$20,$75,$FA,$C8,$A3,$A4,$7A,$23,$26
	.byte	$75,$0E,$C3,$2B,$04,$25,$95,$EF,$64,$9A,$DA,$BF,$9C,$98,$1D,$79
	.byte	$9B,$34,$DC,$E3,$FD,$11,$F9,$00,$40,$CD,$45,$7F,$B6,$7A,$6B,$50
	.byte	$56,$15,$71,$CB,$A1,$17,$0F,$95,$D2,$7F,$E0,$5E,$E9,$44,$61,$27
	.byte	$05,$1B,$44,$EC,$FE,$CB,$D3,$B9,$00

qr092:	.byte	33
	.byte	$FE,$F7,$66,$3F,$C1,$0F,$8D,$10,$6E,$95,$85,$0B,$B7,$46,$68,$B5
	.byte	$DB,$A3,$D5,$62,$EC,$12,$B1,$39,$07,$FA,$AA,$AA,$FE,$01,$94,$75
	.byte	$00,$DA,$4F,$98,$A0,$C8,$D1,$29,$AC,$93,$B2,$81,$CD,$30,$80,$6B
	.byte	$05,$C3,$3E,$68,$9A,$5D,$A1,$40,$1A,$A1,$5D,$8E,$C8,$71,$AB,$9E
	.byte	$56,$C5,$CB,$79,$9A,$ED,$C0,$E8,$9C,$7C,$C0,$CB,$DC,$7A,$28,$16
	.byte	$80,$02,$F2,$AF,$5C,$81,$05,$AA,$6B,$1A,$DA,$4D,$56,$1C,$8F,$02
	.byte	$7A,$D2,$C4,$DA,$DD,$39,$F9,$00,$43,$F1,$45,$BF,$9F,$76,$EB,$10
	.byte	$4A,$26,$71,$DB,$AA,$21,$9F,$C5,$D6,$FE,$F8,$3E,$E8,$47,$E1,$27
	.byte	$05,$96,$34,$2F,$FE,$AF,$41,$F0,$00

qr093:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$38,$50,$6E,$92,$CA,$BB,$75,$9C,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$57,$6E,$A1,$FC,$7E,$92,$50,$61,$11,$E4,$30,$2B,$ED,$FD,$06,$56
	.byte	$7F,$59,$D7,$2E,$36,$BE,$9E,$07,$32,$82,$C2,$E7,$48,$24,$AE,$A7
	.byte	$DF,$6F,$91,$C3,$59,$18,$95,$29,$2A,$2E,$7C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$42,$03,$19,$BA,$DB,$CF,$B5,$D4,$F4,$80,$6E,$A3
	.byte	$FC,$F5,$05,$E7,$1A,$AF,$EE,$75,$6E,$00

qr094:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$18,$50,$6E,$9C,$CA,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$50,$A8,$C1,$FC,$53,$8B,$D0,$61,$8A,$34,$30,$27,$2E,$7D,$06,$6C
	.byte	$E9,$59,$D6,$8C,$96,$BE,$9A,$85,$32,$82,$33,$27,$48,$27,$E8,$B7
	.byte	$DF,$68,$A2,$43,$59,$6A,$65,$29,$2B,$25,$BC,$FB,$80,$4F,$5C,$7F
	.byte	$FB,$1C,$6B,$90,$49,$43,$1A,$BA,$D7,$CF,$A5,$D6,$84,$80,$6E,$B1
	.byte	$7C,$F5,$05,$E3,$1A,$AF,$E9,$F5,$6E,$00

qr095:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$D7,$10,$6E,$83,$43,$8B,$B7,$5D,$68,$75
	.byte	$DB,$A4,$1B,$F2,$EC,$16,$C0,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$4D
	.byte	$00,$FB,$D0,$92,$D5,$16,$97,$AE,$B0,$D7,$C9,$F3,$84,$0F,$11,$A3
	.byte	$E6,$43,$BE,$B5,$AC,$83,$A2,$0F,$79,$2D,$FC,$E1,$01,$55,$42,$E5
	.byte	$2A,$B4,$9A,$90,$4B,$C9,$68,$87,$BF,$F2,$E3,$D9,$D2,$A1,$52,$21
	.byte	$95,$8C,$C4,$B4,$BE,$25,$94,$07,$07,$EB,$1E,$6A,$E7,$18,$1D,$18
	.byte	$A6,$4C,$FC,$A7,$F1,$4A,$FC,$80,$57,$4F,$45,$7F,$B9,$EA,$6B,$50
	.byte	$4D,$90,$B1,$FB,$A8,$BD,$8F,$95,$D5,$04,$C0,$DE,$EB,$E5,$8C,$91
	.byte	$05,$E5,$4C,$CC,$FE,$B0,$73,$B9,$00

qr096:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$3C,$50,$6E,$92,$4A,$BB,$75,$9C,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$54,$82,$81,$FC,$74,$D2,$D0,$60,$B2,$E4,$30,$2E,$6D,$FD,$06,$0E
	.byte	$DD,$59,$D5,$FC,$36,$BE,$98,$B3,$32,$82,$5E,$C7,$48,$24,$E2,$A7
	.byte	$DF,$64,$E8,$C3,$59,$1A,$51,$29,$2B,$3C,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FB,$2C,$6B,$90,$46,$43,$19,$BA,$9F,$CF,$B5,$D7,$C4,$80,$6E,$A0
	.byte	$FC,$F5,$05,$43,$1A,$AF,$EE,$F5,$6E,$00

qr097:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$08,$50,$6E,$9C,$8A,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$52,$A4,$C1,$FC,$5C,$CB,$50,$60,$39,$30,$30,$24,$26,$7D,$06,$3C
	.byte	$E9,$59,$D4,$AF,$96,$BE,$89,$B5,$32,$82,$AA,$67,$48,$26,$88,$87
	.byte	$DF,$67,$C2,$C3,$59,$19,$65,$29,$28,$A7,$FC,$FB,$80,$6C,$5C,$7F
	.byte	$FA,$24,$6B,$90,$47,$43,$1A,$BA,$A7,$CF,$A5,$D6,$A4,$80,$6E,$B1
	.byte	$7C,$F5,$05,$C7,$1A,$AF,$EE,$F5,$6E,$00

qr098:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$08,$50,$6E,$82,$8A,$BB,$75,$DC,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$55,$C9,$51,$FC,$6F,$EB,$50,$60,$F2,$24,$30,$25,$B6,$7D,$06,$22
	.byte	$C8,$59,$D7,$88,$86,$BE,$97,$B5,$72,$82,$F3,$0F,$48,$25,$AC,$97
	.byte	$DF,$67,$B1,$C3,$59,$2B,$A5,$29,$29,$F4,$5C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$46,$03,$1A,$BA,$DF,$CF,$B5,$D5,$D4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EE,$75,$6E,$00

qr099:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$C7,$10,$6E,$83,$47,$8B,$B7,$5D,$6B,$75
	.byte	$DB,$A4,$1C,$F2,$EC,$16,$C1,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$6D
	.byte	$00,$FB,$D0,$4A,$D5,$4C,$97,$8E,$B0,$C2,$A9,$CB,$84,$1E,$91,$95
	.byte	$E6,$4B,$A6,$BC,$2C,$85,$AE,$04,$F9,$2F,$FE,$67,$81,$55,$2B,$E6
	.byte	$5A,$B4,$BF,$11,$4B,$C9,$7E,$87,$5F,$F2,$D2,$E9,$E2,$A1,$4B,$81
	.byte	$90,$8C,$CA,$72,$BD,$25,$95,$2E,$04,$EB,$1E,$89,$E5,$98,$1D,$2A
	.byte	$65,$0C,$FC,$AE,$90,$4A,$FC,$80,$67,$0F,$45,$7F,$B1,$DA,$6B,$50
	.byte	$45,$81,$B1,$EB,$AA,$BE,$8F,$8D,$D5,$07,$C0,$DE,$EA,$E7,$0C,$91
	.byte	$05,$A6,$6C,$CC,$FE,$91,$D3,$B9,$00

qr100:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$F7,$10,$6E,$83,$4F,$8B,$B7,$5D,$6B,$75
	.byte	$DB,$A4,$1A,$F2,$EC,$16,$C0,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$1D
	.byte	$00,$FB,$D0,$4A,$D5,$64,$17,$8E,$B0,$D3,$A9,$D3,$84,$01,$11,$95
	.byte	$E6,$4D,$E6,$BC,$AC,$82,$46,$05,$F9,$2D,$28,$E6,$01,$55,$A9,$E6
	.byte	$1A,$B4,$42,$11,$4B,$C9,$6A,$97,$7F,$F2,$F6,$91,$F2,$A1,$4F,$B9
	.byte	$97,$8C,$C0,$A4,$BC,$25,$94,$E2,$05,$EB,$1E,$FF,$E5,$18,$1D,$49
	.byte	$25,$3C,$FC,$8F,$50,$52,$FC,$80,$67,$1F,$45,$7F,$B9,$C2,$6B,$50
	.byte	$41,$80,$B1,$EB,$AC,$BF,$0F,$8D,$D5,$07,$C0,$DE,$EA,$67,$2C,$91
	.byte	$05,$26,$6C,$CC,$FE,$F1,$D3,$B9,$00

qr101:	.byte	29
	.byte	$FE,$13,$13,$FC,$11,$EB,$D0,$6E,$AA,$08,$BB,$74,$A4,$25,$DB,$A7
	.byte	$37,$2E,$C1,$1C,$D9,$07,$FA,$AA,$AF,$E0,$11,$9D,$00,$EF,$85,$DE
	.byte	$23,$6E,$52,$72,$54,$8A,$B3,$EF,$1A,$DC,$D3,$A9,$F3,$C5,$E5,$CA
	.byte	$13,$61,$34,$AF,$38,$86,$78,$34,$BC,$BA,$F6,$24,$C6,$1B,$CD,$44
	.byte	$51,$6B,$D8,$A0,$D6,$92,$6D,$CA,$A9,$64,$44,$F8,$00,$64,$64,$5F
	.byte	$FA,$B2,$6B,$70,$59,$8D,$11,$BA,$9C,$4F,$8D,$D3,$37,$0E,$6E,$AA
	.byte	$1F,$7B,$05,$EB,$F9,$2F,$E9,$CD,$8D,$80

qr102:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$08,$50,$6E,$9C,$EA,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$56,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$51,$E4,$C1,$FC,$62,$AA,$D0,$60,$4B,$34,$30,$2B,$A6,$7D,$06,$2C
	.byte	$89,$59,$D5,$6A,$96,$BE,$83,$85,$32,$82,$2A,$A3,$48,$27,$E9,$B7
	.byte	$DF,$69,$DB,$C3,$59,$13,$E5,$29,$29,$35,$FC,$FB,$80,$5D,$5C,$7F
	.byte	$FA,$94,$6B,$90,$4F,$43,$1A,$BA,$9F,$CF,$A5,$D4,$A4,$80,$6E,$B2
	.byte	$7C,$F5,$05,$87,$1A,$AF,$EB,$F5,$6E,$00

qr103:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$58,$50,$6E,$84,$EA,$BB,$75,$CC,$C5,$DB,$A5
	.byte	$4F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$53,$CD,$A1,$FC,$67,$88,$D0,$60,$39,$24,$30,$2D,$BE,$7D,$06,$04
	.byte	$0A,$59,$D7,$DA,$A6,$BE,$8A,$9A,$72,$82,$52,$3F,$48,$26,$06,$E7
	.byte	$DF,$64,$AB,$C3,$59,$5A,$71,$29,$28,$3C,$7C,$FB,$80,$78,$5C,$7F
	.byte	$FA,$9C,$6B,$90,$49,$43,$1B,$BA,$C7,$CF,$B5,$D6,$94,$80,$6E,$B3
	.byte	$7C,$F5,$05,$A7,$1A,$AF,$EF,$D5,$6E,$00

qr104:	.byte	33
	.byte	$FE,$77,$16,$3F,$C1,$0C,$A5,$10,$6E,$9C,$A3,$8B,$B7,$56,$79,$B5
	.byte	$DB,$AA,$F1,$F2,$EC,$13,$31,$21,$07,$FA,$AA,$AA,$FE,$00,$9B,$65
	.byte	$00,$C7,$6B,$CA,$8C,$56,$51,$B9,$AC,$A5,$D6,$6B,$84,$17,$06,$40
	.byte	$64,$4C,$F6,$78,$9A,$5D,$AD,$7F,$D9,$AD,$48,$1F,$21,$54,$2A,$5E
	.byte	$66,$C5,$FE,$AB,$CB,$C9,$38,$C0,$DD,$FA,$F2,$EB,$EC,$7A,$2F,$0A
	.byte	$75,$0E,$C2,$23,$04,$25,$95,$88,$6D,$9A,$DA,$8B,$9C,$98,$1D,$18
	.byte	$1A,$34,$DC,$E6,$9D,$11,$F9,$00,$40,$FD,$45,$7F,$BE,$62,$6B,$50
	.byte	$56,$15,$71,$CB,$A1,$1D,$0F,$8D,$D2,$7F,$E0,$5E,$E9,$47,$61,$27
	.byte	$05,$1B,$54,$EC,$FE,$EA,$73,$B9,$00

qr105:	.byte	29
	.byte	$FE,$42,$43,$FC,$13,$30,$50,$6E,$92,$EA,$BB,$75,$5B,$D5,$DB,$A9
	.byte	$2F,$AE,$C1,$32,$61,$07,$FA,$AA,$AF,$E0,$0A,$73,$00,$C7,$66,$50
	.byte	$C6,$E5,$8D,$8D,$94,$D2,$D0,$60,$9A,$62,$38,$0D,$EE,$D0,$B0,$9A
	.byte	$9F,$DB,$DF,$68,$3E,$BE,$88,$8B,$03,$45,$9B,$E7,$48,$24,$AC,$9F
	.byte	$FF,$FE,$8D,$75,$83,$11,$D3,$21,$0B,$24,$7C,$FB,$80,$4B,$9C,$63
	.byte	$FB,$3C,$6B,$90,$56,$63,$1B,$BA,$49,$1F,$DD,$D3,$EC,$A0,$EE,$82
	.byte	$FC,$F5,$05,$30,$06,$DF,$EC,$F5,$6E,$00

qr106:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$0C,$50,$6E,$82,$EA,$BB,$75,$DC,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6C,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$54,$CD,$51,$FC,$64,$CA,$50,$61,$43,$20,$30,$2A,$A6,$7D,$06,$38
	.byte	$0A,$59,$D6,$C8,$8E,$BE,$94,$AD,$32,$82,$46,$AF,$48,$27,$EC,$87
	.byte	$DF,$6F,$F0,$C3,$59,$28,$65,$29,$2B,$F8,$5C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$4A,$03,$1A,$BA,$9F,$CF,$B5,$D6,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EA,$75,$6E,$00

qr107:	.byte	29
	.byte	$FE,$50,$9B,$FC,$16,$0C,$50,$6E,$9C,$EA,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$56,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$55,$80,$E1,$FC,$4F,$AA,$50,$61,$23,$34,$30,$24,$7E,$7D,$06,$60
	.byte	$8A,$59,$D7,$4F,$1E,$BE,$96,$01,$72,$82,$9B,$43,$48,$27,$ED,$B7
	.byte	$DF,$6F,$8B,$C3,$59,$6B,$E5,$29,$29,$25,$FC,$FB,$80,$7C,$5C,$7F
	.byte	$FB,$94,$6B,$90,$4F,$03,$1A,$BA,$87,$CF,$A5,$D5,$94,$80,$6E,$B1
	.byte	$7C,$F5,$05,$23,$1A,$AF,$EB,$F5,$6E,$00

qr108:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$0C,$50,$6E,$92,$EA,$BB,$75,$9C,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$57,$26,$91,$FC,$43,$D3,$50,$60,$B0,$E4,$30,$23,$6D,$BD,$06,$70
	.byte	$5F,$59,$D7,$6F,$3E,$BE,$85,$AB,$72,$82,$EB,$A7,$48,$26,$C6,$87
	.byte	$DF,$61,$A9,$43,$59,$19,$95,$29,$28,$32,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FB,$24,$6B,$90,$42,$43,$19,$BA,$9F,$CF,$B5,$D4,$D4,$80,$6E,$B9
	.byte	$7C,$F5,$05,$33,$1A,$AF,$EE,$75,$6E,$00

qr109:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$48,$95,$50,$6E,$91,$15,$4B,$B7,$43,$25,$E5
	.byte	$DB,$A9,$98,$42,$EC,$17,$73,$65,$07,$FA,$AA,$AA,$FE,$01,$8B,$44
	.byte	$00,$E6,$DD,$09,$F9,$D8,$44,$EC,$F9,$D6,$DB,$D7,$5F,$79,$37,$43
	.byte	$74,$06,$2E,$78,$9A,$5B,$2D,$3C,$DD,$BD,$0E,$45,$4C,$E3,$B0,$8B
	.byte	$53,$90,$02,$9D,$18,$A4,$86,$C4,$CD,$BB,$CE,$CB,$FE,$7A,$24,$07
	.byte	$75,$1E,$8A,$36,$68,$93,$4A,$01,$39,$CF,$8F,$08,$C7,$F5,$AA,$73
	.byte	$4A,$15,$D8,$EF,$5D,$09,$F9,$00,$54,$CD,$44,$7F,$9B,$D4,$AA,$30
	.byte	$53,$44,$31,$9B,$A0,$70,$BF,$D5,$D2,$3D,$E4,$4E,$EB,$C6,$C1,$27
	.byte	$05,$4B,$15,$E8,$FE,$BC,$A8,$D4,$80

qr110:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$0C,$50,$6E,$92,$EA,$BB,$75,$9C,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$51,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$51,$02,$A1,$FC,$6B,$92,$D0,$61,$00,$E0,$30,$2B,$AD,$FD,$06,$40
	.byte	$1E,$59,$D4,$7A,$A6,$BE,$9F,$37,$72,$82,$96,$27,$48,$24,$82,$97
	.byte	$DF,$6E,$A8,$43,$59,$2B,$51,$29,$2B,$6E,$7C,$FB,$80,$58,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$42,$43,$19,$BA,$DF,$CF,$B5,$D5,$C4,$80,$6E,$B8
	.byte	$FC,$F5,$05,$03,$1A,$AF,$E8,$F5,$6E,$00

qr111:	.byte	33
	.byte	$FE,$4C,$C3,$3F,$C1,$4F,$92,$D0,$6E,$BF,$36,$4B,$B7,$4E,$E6,$45
	.byte	$DB,$A8,$6E,$32,$EC,$16,$B1,$85,$07,$FA,$AA,$AA,$FE,$01,$94,$FA
	.byte	$00,$D3,$21,$95,$BB,$3E,$99,$A6,$53,$4A,$F8,$0C,$98,$6D,$10,$54
	.byte	$FA,$32,$20,$85,$4F,$0B,$4B,$C0,$65,$5F,$8B,$21,$3D,$24,$71,$DE
	.byte	$89,$3A,$7E,$B6,$57,$B8,$E4,$D0,$73,$83,$1A,$E1,$19,$2F,$6D,$8A
	.byte	$89,$FD,$0E,$ED,$A0,$54,$52,$60,$E6,$65,$27,$6D,$78,$69,$DA,$A8
	.byte	$79,$4D,$3B,$86,$93,$DC,$FC,$00,$7B,$6E,$C6,$7F,$AD,$AB,$AA,$50
	.byte	$4A,$0A,$91,$0B,$A4,$CE,$CF,$95,$D6,$76,$07,$C2,$E8,$E9,$14,$73
	.byte	$05,$17,$9B,$D0,$FE,$80,$14,$A5,$00

qr112:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$0C,$50,$6E,$92,$EA,$BB,$75,$9C,$C5,$DB,$A1
	.byte	$4F,$AE,$C1,$53,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$51,$6A,$A1,$FC,$58,$D2,$50,$61,$F0,$E4,$30,$27,$AD,$FD,$06,$26
	.byte	$5D,$59,$D5,$CF,$AE,$BE,$8B,$1F,$32,$82,$A2,$43,$48,$26,$87,$B7
	.byte	$DF,$6A,$A1,$C3,$59,$72,$15,$29,$2B,$B0,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$42,$43,$19,$BA,$DF,$CF,$B5,$D4,$C4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$E7,$1A,$AF,$EB,$F5,$6E,$00

qr113:	.byte	33
	.byte	$FE,$77,$26,$3F,$C1,$0C,$A5,$10,$6E,$9C,$AB,$8B,$B7,$56,$68,$B5
	.byte	$DB,$AA,$F3,$F2,$EC,$13,$37,$61,$07,$FA,$AA,$AA,$FE,$00,$98,$15
	.byte	$00,$C7,$6B,$52,$8C,$0C,$11,$39,$AC,$9C,$96,$0B,$84,$17,$96,$77
	.byte	$64,$4B,$2E,$68,$1A,$5C,$C9,$71,$59,$AE,$49,$9C,$21,$55,$B8,$1E
	.byte	$16,$C5,$5F,$0B,$DB,$C9,$1C,$D0,$CD,$FA,$F1,$E3,$CC,$7A,$35,$86
	.byte	$45,$0E,$C4,$F7,$14,$25,$97,$EC,$68,$9A,$DA,$C9,$1F,$98,$1D,$0B
	.byte	$5B,$34,$DC,$E3,$3D,$29,$F9,$00,$50,$FD,$45,$7F,$BE,$52,$6B,$50
	.byte	$56,$26,$71,$DB,$A3,$05,$0F,$8D,$D2,$78,$E0,$5E,$E8,$C7,$C1,$27
	.byte	$05,$1A,$14,$EC,$FE,$CB,$D3,$B9,$00

qr114:	.byte	33
	.byte	$FE,$28,$BD,$3F,$C1,$50,$D7,$10,$6E,$82,$A3,$8B,$B7,$5D,$FE,$75
	.byte	$DB,$A4,$2B,$F2,$EC,$16,$FA,$41,$07,$FA,$AA,$AA,$FE,$00,$E1,$1D
	.byte	$00,$FB,$D0,$4A,$D5,$76,$16,$0E,$B0,$FC,$C9,$D9,$84,$10,$81,$82
	.byte	$E6,$46,$66,$B9,$AC,$87,$2E,$06,$F9,$2D,$3E,$E7,$21,$55,$0A,$26
	.byte	$1A,$B4,$A2,$71,$C3,$C9,$10,$07,$4F,$F2,$FF,$99,$D8,$A1,$50,$39
	.byte	$95,$8C,$C2,$F6,$A4,$A5,$97,$22,$01,$EB,$1E,$2C,$E6,$18,$1D,$63
	.byte	$A7,$3C,$FC,$92,$51,$52,$FC,$80,$47,$4F,$45,$7F,$A1,$F2,$6B,$50
	.byte	$41,$97,$B1,$EB,$AC,$B5,$0F,$9D,$D4,$05,$C0,$DE,$EB,$E4,$2C,$91
	.byte	$05,$E5,$1C,$CC,$FE,$B0,$53,$B9,$00

qr115:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$7C,$50,$6E,$9C,$EA,$BB,$75,$7C,$C5,$DB,$A5
	.byte	$CF,$AE,$C1,$67,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$53,$47,$A1,$FC,$4C,$89,$D0,$61,$EB,$90,$30,$24,$2C,$3D,$06,$54
	.byte	$4B,$59,$D4,$5C,$3E,$BE,$8B,$86,$72,$82,$EA,$5F,$48,$27,$06,$D7
	.byte	$DF,$69,$F2,$43,$59,$39,$41,$29,$29,$3B,$FC,$FB,$80,$48,$5C,$7F
	.byte	$FA,$04,$6B,$90,$46,$43,$18,$BA,$BF,$CF,$A5,$D6,$94,$80,$6E,$B2
	.byte	$FC,$F5,$05,$F3,$1A,$AF,$EE,$75,$6E,$00

qr116:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$7C,$50,$6E,$9C,$EA,$BB,$75,$7C,$C5,$DB,$A5
	.byte	$AF,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$55,$67,$91,$FC,$76,$C9,$50,$61,$11,$94,$30,$24,$FC,$7D,$06,$18
	.byte	$2A,$59,$D6,$6C,$36,$BE,$97,$9E,$72,$82,$C6,$BF,$48,$26,$C2,$C7
	.byte	$DF,$6D,$D0,$43,$59,$13,$35,$29,$2B,$B3,$FC,$FB,$80,$79,$5C,$7F
	.byte	$FA,$0C,$6B,$90,$4F,$03,$18,$BA,$AF,$CF,$A5,$D7,$94,$80,$6E,$A0
	.byte	$FC,$F5,$05,$D3,$1A,$AF,$EC,$F5,$6E,$00

qr117:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$6C,$50,$6E,$84,$6A,$BB,$75,$C8,$C5,$DB,$A5
	.byte	$4F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CA,$55
	.byte	$53,$E9,$B1,$FC,$61,$A9,$D0,$61,$69,$24,$30,$23,$A6,$7D,$06,$28
	.byte	$A8,$59,$D6,$DA,$36,$BE,$83,$06,$72,$82,$3E,$DF,$48,$24,$CA,$E7
	.byte	$DF,$67,$83,$C3,$59,$03,$35,$29,$28,$EE,$7C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$1C,$6B,$90,$4D,$43,$1B,$BA,$E7,$CF,$B5,$D5,$A4,$80,$6E,$AA
	.byte	$FC,$F5,$05,$23,$1A,$AF,$EB,$D5,$6E,$00

qr118:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$2C,$50,$6E,$9C,$EA,$BB,$75,$48,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DA,$55
	.byte	$54,$C8,$F1,$FC,$44,$AB,$D0,$60,$AA,$34,$30,$24,$6E,$7D,$06,$04
	.byte	$09,$59,$D7,$59,$06,$BE,$9E,$19,$32,$82,$82,$A7,$48,$25,$2B,$87
	.byte	$DF,$6A,$A2,$43,$59,$30,$E5,$29,$2A,$75,$FC,$FB,$80,$4C,$5C,$7F
	.byte	$FA,$B4,$6B,$90,$42,$43,$1A,$BA,$BF,$CF,$A5,$D4,$A4,$80,$6E,$B0
	.byte	$FC,$F5,$05,$17,$1A,$AF,$EE,$F5,$6E,$00

qr119:	.byte	29
	.byte	$FE,$4B,$13,$FC,$14,$CF,$50,$6E,$BE,$DA,$BB,$74,$E4,$25,$DB,$A8
	.byte	$5E,$2E,$C1,$69,$FD,$07,$FA,$AA,$AF,$E0,$19,$0F,$00,$D3,$2F,$93
	.byte	$B0,$21,$82,$72,$56,$FA,$17,$7D,$73,$3C,$41,$E3,$20,$45,$E5,$8E
	.byte	$96,$28,$13,$8A,$C1,$A2,$FC,$8A,$FC,$BA,$8F,$28,$54,$52,$22,$F6
	.byte	$18,$6A,$98,$A0,$D6,$6B,$54,$EE,$3A,$63,$40,$FA,$00,$64,$64,$5F
	.byte	$FB,$20,$2A,$50,$4F,$C5,$16,$BA,$1C,$4F,$8D,$D5,$13,$9C,$2E,$9E
	.byte	$0D,$33,$05,$8F,$F9,$2F,$EC,$04,$A9,$00

qr120:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$D8,$50,$6E,$9C,$AA,$BB,$75,$BC,$C5,$DB,$A2
	.byte	$AF,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$55,$CE,$91,$FC,$5C,$B0,$D0,$60,$60,$D0,$30,$22,$64,$FD,$06,$2C
	.byte	$59,$59,$D4,$3F,$0E,$BE,$96,$0E,$72,$82,$3E,$A7,$48,$24,$4A,$A7
	.byte	$DF,$68,$B3,$C3,$59,$62,$61,$29,$29,$3B,$FC,$FB,$80,$6C,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$4B,$43,$18,$BA,$85,$CF,$B5,$D7,$A4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$53,$1A,$AF,$E8,$75,$6E,$00

qr121:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$D3,$10,$6E,$83,$45,$8B,$B7,$5D,$69,$75
	.byte	$DB,$A4,$19,$F2,$EC,$16,$C2,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$2D
	.byte	$00,$FB,$D0,$C2,$D5,$3E,$D7,$BE,$B0,$F8,$E9,$E3,$84,$12,$21,$A7
	.byte	$E6,$47,$6E,$B5,$AC,$86,$22,$0D,$79,$2F,$6D,$E0,$01,$55,$60,$65
	.byte	$1A,$B4,$8B,$70,$4B,$C9,$36,$A7,$8F,$F2,$F1,$81,$D8,$A1,$45,$29
	.byte	$94,$8C,$C4,$2A,$BF,$25,$97,$A5,$07,$EB,$1E,$8D,$E7,$38,$1D,$20
	.byte	$66,$7C,$FC,$8E,$D1,$42,$FC,$80,$47,$4F,$45,$7F,$B1,$EA,$6B,$50
	.byte	$41,$90,$B1,$FB,$A8,$BD,$0F,$95,$D5,$04,$C0,$DE,$EB,$65,$8C,$91
	.byte	$05,$E5,$7C,$CC,$FE,$F0,$73,$B9,$00

qr122:	.byte	33
	.byte	$FE,$22,$73,$3F,$C1,$22,$2B,$D0,$6E,$A4,$46,$0B,$B7,$49,$97,$45
	.byte	$DB,$A4,$C9,$12,$EC,$11,$DD,$8D,$07,$FA,$AA,$AA,$FE,$01,$22,$BE
	.byte	$00,$EF,$88,$CC,$E2,$18,$AE,$E6,$53,$5A,$8E,$E0,$0A,$25,$9D,$DD
	.byte	$DE,$AE,$E3,$35,$4F,$0E,$61,$9B,$F7,$17,$79,$11,$B9,$B6,$42,$62
	.byte	$B9,$3A,$67,$08,$E5,$F1,$F0,$CE,$C7,$11,$4C,$8E,$F3,$2F,$73,$91
	.byte	$DA,$B4,$27,$6D,$3D,$C6,$18,$AB,$9B,$65,$26,$8C,$92,$80,$FE,$D8
	.byte	$E1,$DF,$72,$97,$88,$4C,$FC,$00,$4E,$77,$C6,$FF,$BE,$99,$EB,$70
	.byte	$51,$EA,$91,$3B,$A9,$2F,$EF,$9D,$D2,$94,$4E,$E6,$EB,$91,$34,$73
	.byte	$05,$61,$9F,$42,$FE,$E8,$5D,$81,$80

qr123:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$13,$10,$6E,$9C,$A3,$8B,$B7,$5A,$1A,$75
	.byte	$DB,$A2,$F3,$F2,$EC,$15,$3E,$01,$07,$FA,$AA,$AA,$FE,$00,$1A,$3D
	.byte	$00,$FB,$EB,$42,$D5,$2C,$20,$FE,$B0,$F7,$96,$0B,$84,$07,$0E,$57
	.byte	$E6,$40,$6D,$0D,$AC,$83,$4B,$78,$79,$2C,$BE,$1E,$01,$55,$BB,$9A
	.byte	$5A,$B4,$13,$AB,$6B,$C9,$76,$A0,$FF,$F2,$CB,$FE,$10,$A1,$5D,$B6
	.byte	$54,$8C,$C8,$27,$05,$25,$95,$EC,$78,$EB,$1E,$CC,$1C,$B8,$1D,$09
	.byte	$59,$3C,$FC,$BB,$6B,$C2,$FC,$80,$40,$4F,$45,$7F,$AE,$7A,$6B,$50
	.byte	$42,$64,$B1,$DB,$AF,$17,$0F,$95,$D6,$77,$C0,$DE,$EB,$9F,$0C,$91
	.byte	$05,$19,$7C,$CC,$FE,$CB,$D3,$B9,$00

qr124:	.byte	33
	.byte	$FE,$2C,$1D,$3F,$C1,$53,$B7,$10,$6E,$82,$01,$8B,$B7,$5D,$FA,$75
	.byte	$DB,$A4,$29,$F2,$EC,$16,$FF,$41,$07,$FA,$AA,$AA,$FE,$00,$E6,$1D
	.byte	$00,$FB,$D7,$D2,$D5,$04,$54,$7E,$B0,$D9,$A8,$4B,$84,$17,$11,$53
	.byte	$E6,$4A,$7E,$E5,$AC,$85,$22,$13,$79,$2E,$9C,$7D,$01,$54,$AA,$E3
	.byte	$1A,$B4,$7B,$70,$4B,$C9,$28,$86,$7F,$F2,$C5,$B9,$D0,$A1,$4C,$BD
	.byte	$B4,$8C,$CC,$7C,$A1,$25,$94,$C2,$06,$EB,$1E,$5F,$E7,$38,$1D,$3A
	.byte	$E6,$3C,$FC,$A6,$D1,$42,$FC,$80,$77,$4F,$45,$7F,$A9,$CA,$6B,$50
	.byte	$4D,$84,$B1,$FB,$AE,$B5,$0F,$9D,$D6,$07,$C0,$DE,$EB,$64,$8C,$91
	.byte	$05,$A5,$3C,$CC,$FE,$F0,$53,$B9,$00

qr125:	.byte	33
	.byte	$FE,$22,$13,$3F,$C1,$70,$46,$D0,$6E,$A0,$DA,$4B,$B7,$49,$97,$45
	.byte	$DB,$AE,$80,$32,$EC,$15,$4F,$85,$07,$FA,$AA,$AA,$FE,$01,$6B,$AA
	.byte	$00,$D3,$1A,$8D,$BB,$54,$6E,$C6,$53,$77,$C7,$CC,$98,$6E,$9F,$94
	.byte	$FA,$3D,$33,$3D,$4F,$0B,$4C,$BF,$65,$5F,$4D,$5A,$1D,$25,$C1,$E1
	.byte	$B9,$3A,$2E,$0C,$D7,$B8,$8A,$37,$43,$83,$2D,$C6,$8B,$2F,$72,$05
	.byte	$7C,$FD,$0E,$20,$09,$54,$51,$69,$94,$65,$27,$4E,$02,$49,$DA,$C2
	.byte	$C5,$0D,$3B,$BB,$08,$6C,$FC,$00,$7C,$0E,$C6,$7F,$AA,$23,$AA,$50
	.byte	$4D,$DA,$91,$2B,$A3,$74,$CF,$95,$D6,$01,$07,$C2,$E9,$92,$B4,$73
	.byte	$05,$69,$FB,$D0,$FE,$DA,$14,$A5,$00

qr126:	.byte	33
	.byte	$FE,$2D,$3D,$3F,$C1,$53,$83,$10,$6E,$83,$25,$8B,$B7,$5D,$71,$75
	.byte	$DB,$A4,$0D,$F2,$EC,$16,$CE,$41,$07,$FA,$AA,$AA,$FE,$00,$E6,$0D
	.byte	$00,$FB,$D1,$42,$D5,$42,$17,$5E,$B0,$C3,$E9,$F3,$84,$19,$A1,$91
	.byte	$E6,$4C,$36,$BD,$2C,$80,$E4,$05,$F9,$2D,$AF,$64,$81,$54,$CB,$25
	.byte	$1A,$B4,$D3,$50,$4B,$C9,$40,$67,$3F,$F2,$D9,$91,$C0,$A1,$46,$39
	.byte	$84,$8C,$C0,$7C,$BD,$25,$96,$21,$07,$EB,$1E,$78,$E6,$38,$1D,$50
	.byte	$E6,$7C,$FC,$8F,$71,$C2,$FC,$80,$47,$5F,$45,$7F,$B1,$C2,$6B,$50
	.byte	$49,$90,$B1,$FB,$A8,$A5,$0F,$8D,$D7,$00,$C0,$DE,$EB,$67,$8C,$91
	.byte	$05,$26,$7C,$CC,$FE,$F0,$D3,$B9,$00

qr127:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$F8,$50,$6E,$9C,$AA,$BB,$75,$AC,$C5,$DB,$A2
	.byte	$AF,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$01,$13,$00,$FB,$EE,$55
	.byte	$51,$2E,$01,$FC,$57,$96,$50,$60,$81,$E4,$30,$22,$AD,$FD,$06,$74
	.byte	$B4,$59,$D7,$68,$2E,$BE,$97,$86,$32,$82,$23,$C5,$48,$25,$21,$A7
	.byte	$DF,$62,$E1,$C3,$59,$72,$51,$29,$2A,$6A,$FC,$FB,$80,$79,$5C,$7F
	.byte	$FB,$84,$6B,$90,$42,$43,$19,$BA,$C7,$CF,$AD,$D4,$94,$80,$EE,$B3
	.byte	$7C,$F5,$05,$A3,$1A,$AF,$E9,$D5,$6E,$00

qr128:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$5C,$50,$6E,$9C,$2A,$BB,$75,$7C,$C5,$DB,$A5
	.byte	$8F,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$57,$23,$91,$FC,$4A,$A9,$50,$61,$CA,$94,$30,$23,$FC,$7D,$06,$6A
	.byte	$A9,$59,$D4,$CD,$2E,$BE,$93,$12,$32,$82,$2B,$7F,$48,$24,$89,$C7
	.byte	$DF,$62,$92,$43,$59,$4A,$E5,$29,$2B,$EB,$BC,$FB,$80,$4B,$5C,$7F
	.byte	$FA,$9C,$6B,$90,$45,$43,$18,$BA,$DF,$CF,$A5,$D7,$A4,$80,$6E,$B3
	.byte	$FC,$F5,$05,$87,$1A,$AF,$EC,$F5,$6E,$00

qr129:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$3C,$50,$6E,$9C,$2A,$BB,$75,$4C,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DC,$55
	.byte	$52,$24,$E1,$FC,$72,$8A,$D0,$61,$AA,$34,$30,$2B,$76,$7D,$06,$3A
	.byte	$8A,$59,$D5,$BC,$1E,$BE,$99,$8D,$72,$82,$92,$C7,$48,$26,$6B,$A7
	.byte	$DF,$6D,$FB,$C3,$59,$2B,$E1,$29,$2A,$3D,$DC,$FB,$80,$5E,$5C,$7F
	.byte	$FB,$0C,$6B,$90,$40,$43,$1A,$BA,$8F,$CF,$A5,$D7,$A4,$80,$6E,$A3
	.byte	$FC,$F5,$05,$67,$1A,$AF,$EB,$F5,$6E,$00

qr130:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$6C,$50,$6E,$84,$2A,$BB,$75,$CC,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$62,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$53,$21,$A1,$FC,$67,$E8,$50,$61,$DB,$24,$30,$2A,$6E,$7D,$06,$56
	.byte	$89,$59,$D7,$0C,$2E,$BE,$91,$0E,$32,$82,$F7,$DF,$48,$25,$03,$D7
	.byte	$DF,$63,$A3,$43,$59,$0B,$31,$29,$2B,$E8,$7C,$FB,$80,$4A,$5C,$7F
	.byte	$FB,$9C,$6B,$90,$49,$43,$1B,$BA,$C7,$CF,$B5,$D6,$A4,$80,$6E,$B3
	.byte	$FC,$F5,$05,$23,$1A,$AF,$EB,$D5,$6E,$00

qr131:	.byte	33
	.byte	$FE,$E8,$51,$3F,$C1,$70,$69,$10,$6E,$AD,$7D,$6B,$B7,$5A,$75,$05
	.byte	$DB,$A0,$6E,$32,$EC,$14,$F8,$A1,$07,$FA,$AA,$AA,$FE,$00,$94,$FA
	.byte	$00,$CE,$17,$DE,$97,$F8,$34,$34,$88,$2F,$87,$D3,$67,$9A,$B6,$BC
	.byte	$97,$8E,$22,$C4,$6B,$9F,$2E,$37,$1A,$A3,$28,$82,$AF,$6C,$EB,$94
	.byte	$9D,$A8,$22,$D7,$5F,$B8,$98,$C4,$11,$CA,$32,$FF,$F8,$42,$C6,$9A
	.byte	$8D,$FD,$08,$E0,$CE,$E2,$8F,$A6,$3F,$08,$90,$AE,$04,$B6,$24,$01
	.byte	$17,$8B,$E0,$FA,$16,$4E,$FD,$00,$64,$C1,$45,$BF,$97,$D0,$AA,$D0
	.byte	$5E,$8E,$D1,$3B,$AC,$CD,$4F,$8D,$D2,$3C,$23,$52,$E9,$86,$22,$A9
	.byte	$05,$94,$BB,$D0,$FE,$B7,$6F,$C8,$80

qr132:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$DC,$50,$6E,$9C,$AA,$BB,$75,$BC,$C5,$DB,$A2
	.byte	$AF,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$52,$EA,$91,$FC,$64,$B0,$50,$60,$11,$D0,$30,$29,$B4,$FD,$06,$52
	.byte	$7A,$59,$D5,$DA,$1E,$BE,$8C,$B6,$72,$82,$9A,$E7,$48,$24,$A8,$97
	.byte	$DF,$62,$A3,$43,$59,$53,$E5,$29,$2A,$2B,$FC,$FB,$80,$6D,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$4F,$03,$18,$BA,$A5,$CF,$B5,$D4,$A4,$80,$6E,$B1
	.byte	$FC,$F5,$05,$93,$1A,$AF,$E8,$75,$6E,$00

qr133:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$5C,$50,$6E,$84,$AA,$BB,$75,$CC,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$56,$09,$91,$FC,$43,$89,$D0,$61,$89,$24,$30,$24,$B6,$7D,$06,$46
	.byte	$4A,$59,$D6,$AC,$3E,$BE,$86,$96,$72,$82,$F7,$DF,$48,$26,$0B,$D7
	.byte	$DF,$6A,$CB,$43,$59,$01,$31,$29,$2A,$3A,$7C,$FB,$80,$7A,$5C,$7F
	.byte	$FA,$0C,$6B,$90,$45,$03,$1B,$BA,$C7,$CF,$B5,$D6,$84,$80,$6E,$AA
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$ED,$D5,$6E,$00

qr134:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$08,$50,$6E,$82,$CA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6E,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$50,$AD,$61,$FC,$40,$CA,$D0,$61,$B0,$24,$30,$23,$76,$7D,$06,$54
	.byte	$E8,$59,$D6,$9F,$06,$BE,$95,$19,$72,$82,$F3,$CF,$48,$25,$2E,$A7
	.byte	$DF,$6A,$D9,$C3,$59,$39,$21,$29,$29,$F2,$5C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4A,$03,$1A,$BA,$FF,$CF,$B5,$D5,$D4,$80,$6E,$AA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EA,$75,$6E,$00

qr135:	.byte	33
	.byte	$FE,$2C,$7D,$3F,$C1,$53,$A7,$10,$6E,$82,$09,$8B,$B7,$5D,$F2,$75
	.byte	$DB,$A4,$2D,$F2,$EC,$16,$FE,$41,$07,$FA,$AA,$AA,$FE,$00,$E6,$3D
	.byte	$00,$FB,$D7,$DA,$D5,$5E,$14,$46,$B0,$E8,$E8,$43,$84,$1E,$11,$55
	.byte	$E6,$4B,$2E,$E4,$2C,$84,$C8,$13,$F9,$2E,$8F,$7C,$A1,$55,$33,$E3
	.byte	$6A,$B4,$E2,$F1,$43,$C9,$3A,$16,$4F,$F2,$D7,$81,$C2,$A1,$5D,$19
	.byte	$90,$8C,$C9,$FE,$A4,$25,$95,$8A,$07,$EB,$1E,$B9,$E7,$18,$1D,$7A
	.byte	$A6,$5C,$FC,$A7,$B1,$52,$FC,$80,$77,$5F,$45,$7F,$B1,$C2,$6B,$50
	.byte	$4D,$B4,$B1,$FB,$A8,$A4,$0F,$9D,$D6,$05,$C0,$DE,$EA,$E4,$0C,$91
	.byte	$05,$A4,$0C,$CC,$FE,$F1,$CB,$B9,$00

qr136:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$F7,$10,$6E,$83,$47,$8B,$B7,$5D,$60,$75
	.byte	$DB,$A4,$1D,$72,$EC,$16,$C2,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$0D
	.byte	$00,$FB,$D0,$C2,$D5,$5A,$17,$9E,$B0,$E6,$E9,$F9,$84,$0B,$91,$A6
	.byte	$E6,$4F,$2E,$B4,$AC,$84,$4C,$0F,$F9,$2C,$2A,$61,$21,$55,$62,$A5
	.byte	$3A,$B4,$BF,$D0,$53,$C9,$26,$17,$AF,$F2,$FE,$A1,$C8,$A1,$58,$A1
	.byte	$95,$8C,$CC,$38,$BE,$25,$97,$C8,$07,$EB,$1E,$58,$E7,$18,$1D,$43
	.byte	$26,$7C,$FC,$B2,$11,$52,$FC,$80,$67,$4F,$45,$7F,$B1,$EA,$6B,$50
	.byte	$49,$90,$B1,$FB,$A8,$BC,$0F,$95,$D5,$04,$C0,$DE,$EA,$E5,$8C,$91
	.byte	$05,$E5,$4C,$CC,$FE,$90,$6B,$B9,$00

qr137:	.byte	29
	.byte	$FE,$F6,$43,$FC,$10,$E0,$90,$6E,$95,$EE,$BB,$74,$68,$D5,$DB,$A3
	.byte	$EB,$2E,$C1,$2B,$01,$07,$FA,$AA,$AF,$E0,$19,$F0,$00,$DA,$4A,$C2
	.byte	$0D,$49,$1D,$8D,$B2,$B3,$42,$28,$63,$07,$BE,$1A,$F6,$90,$B0,$F8
	.byte	$F5,$D7,$EC,$4C,$2C,$F7,$B8,$9A,$03,$45,$FE,$97,$01,$05,$8F,$29
	.byte	$E7,$BB,$F4,$75,$83,$F8,$5F,$11,$CC,$B2,$B5,$FF,$00,$68,$9C,$63
	.byte	$F9,$CD,$6B,$10,$4C,$7B,$1A,$BA,$D1,$1F,$C5,$D4,$AC,$63,$6E,$82
	.byte	$58,$67,$05,$24,$06,$DF,$EE,$F1,$FC,$00

qr138:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$D8,$50,$6E,$9C,$CA,$BB,$75,$BF,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$56,$22,$91,$FC,$4C,$F1,$50,$60,$F1,$D4,$30,$20,$B4,$FD,$06,$70
	.byte	$F8,$59,$D6,$4A,$86,$BE,$93,$3E,$72,$82,$BB,$87,$48,$27,$4D,$97
	.byte	$DF,$62,$C2,$C3,$59,$20,$21,$29,$2B,$25,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$4F,$43,$18,$BA,$85,$CF,$B5,$D7,$A4,$80,$6E,$A1
	.byte	$FC,$F5,$05,$53,$1A,$AF,$E8,$75,$6E,$00

qr139:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$58,$50,$6E,$9C,$CA,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$CF,$AE,$C1,$67,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$57,$27,$91,$FC,$5F,$C8,$50,$60,$22,$90,$30,$20,$EC,$7D,$06,$26
	.byte	$AA,$59,$D5,$9D,$AE,$BE,$83,$AA,$32,$82,$5F,$1F,$48,$24,$2B,$C7
	.byte	$DF,$62,$C0,$C3,$59,$2B,$65,$29,$2A,$B4,$BC,$FB,$80,$7B,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$42,$43,$18,$BA,$BF,$CF,$A5,$D6,$94,$80,$6E,$B8
	.byte	$7C,$F5,$05,$A3,$1A,$AF,$EB,$F5,$6E,$00

qr140:	.byte	29
	.byte	$FE,$70,$9B,$FC,$15,$58,$50,$6E,$9C,$CA,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$AF,$AE,$C1,$66,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$53,$EB,$91,$FC,$4E,$A8,$D0,$61,$12,$90,$30,$2C,$E4,$3D,$06,$64
	.byte	$0B,$59,$D6,$48,$BE,$BE,$98,$1A,$72,$82,$3A,$9F,$48,$27,$0B,$D7
	.byte	$DF,$67,$CA,$43,$59,$08,$C1,$29,$2A,$3F,$FC,$FB,$80,$58,$5C,$7F
	.byte	$FA,$04,$6B,$90,$4E,$43,$18,$BA,$9F,$CF,$A5,$D5,$94,$80,$6E,$AA
	.byte	$FC,$F5,$05,$73,$1A,$AF,$EC,$75,$6E,$00

qr141:	.byte	33
	.byte	$FE,$F7,$46,$3F,$C1,$48,$A5,$50,$6E,$91,$11,$4B,$B7,$43,$34,$E5
	.byte	$DB,$A9,$98,$C2,$EC,$17,$77,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$54
	.byte	$00,$E6,$DD,$89,$F9,$F6,$44,$4C,$F9,$CD,$9B,$A5,$5F,$7B,$17,$73
	.byte	$74,$07,$FE,$68,$9A,$5B,$6F,$32,$DD,$BF,$69,$C7,$EC,$E3,$72,$8B
	.byte	$53,$90,$87,$BD,$10,$A4,$9E,$F4,$DD,$BB,$E8,$AB,$C6,$7A,$2B,$23
	.byte	$43,$1E,$8A,$FE,$78,$93,$4A,$2D,$3C,$CF,$8F,$18,$C5,$55,$AA,$73
	.byte	$CB,$45,$D8,$CA,$7D,$39,$F9,$00,$54,$ED,$44,$7F,$8B,$E6,$AA,$30
	.byte	$57,$70,$31,$8B,$A2,$68,$BF,$D5,$D2,$39,$E4,$4E,$EB,$47,$E1,$27
	.byte	$05,$8A,$45,$E8,$FE,$FD,$10,$D4,$80

qr142:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$48,$50,$6E,$9C,$4A,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$8F,$AE,$C1,$64,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$54,$6F,$91,$FC,$40,$C9,$50,$61,$D0,$94,$30,$22,$3C,$7D,$06,$34
	.byte	$28,$59,$D7,$EB,$AE,$BE,$99,$2A,$72,$82,$6A,$DF,$48,$24,$A3,$D7
	.byte	$DF,$6B,$F1,$43,$59,$5A,$41,$29,$2B,$24,$BC,$FB,$80,$5B,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$47,$43,$18,$BA,$87,$CF,$A5,$D4,$84,$80,$6E,$A9
	.byte	$FC,$F5,$05,$13,$1A,$AF,$EB,$F5,$6E,$00

qr143:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$08,$50,$6E,$82,$4A,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$54,$C9,$51,$FC,$55,$AA,$D0,$60,$73,$20,$30,$20,$7E,$7D,$06,$3A
	.byte	$C8,$59,$D6,$3E,$86,$BE,$87,$95,$72,$82,$EB,$6F,$48,$26,$C4,$97
	.byte	$DF,$63,$C1,$43,$59,$61,$A1,$29,$28,$72,$5C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$42,$03,$1A,$BA,$BF,$CF,$B5,$D6,$D4,$80,$6E,$B2
	.byte	$7C,$F5,$05,$37,$1A,$AF,$EE,$75,$6E,$00

qr144:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$08,$50,$6E,$82,$CA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$52,$ED,$61,$FC,$5E,$CA,$D0,$60,$A9,$24,$30,$23,$EE,$7D,$06,$08
	.byte	$28,$59,$D4,$98,$86,$BE,$8B,$29,$72,$82,$46,$AF,$48,$27,$EE,$A7
	.byte	$DF,$6F,$89,$C3,$59,$4B,$21,$29,$2A,$3E,$5C,$FB,$80,$7A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4A,$03,$1A,$BA,$BF,$CF,$B5,$D5,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EA,$75,$6E,$00

qr145:	.byte	29
	.byte	$FE,$44,$9B,$FC,$16,$C8,$50,$6E,$9C,$CA,$BB,$75,$BF,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E2,$55
	.byte	$54,$AE,$B1,$FC,$5A,$B1,$D0,$61,$41,$D4,$30,$22,$BC,$FD,$06,$4C
	.byte	$99,$59,$D6,$FE,$86,$BE,$9D,$1A,$32,$82,$72,$E7,$48,$25,$22,$87
	.byte	$DF,$69,$BA,$C3,$59,$6B,$25,$29,$2A,$EB,$FC,$FB,$80,$5E,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$4F,$03,$18,$BA,$85,$CF,$B5,$D7,$A4,$80,$6E,$B1
	.byte	$FC,$F5,$05,$D3,$1A,$AF,$EE,$75,$6E,$00

qr146:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$08,$50,$6E,$9C,$CA,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$53,$2C,$C1,$FC,$68,$EA,$D0,$61,$21,$34,$30,$27,$F6,$3D,$06,$66
	.byte	$8B,$59,$D7,$3D,$9E,$BE,$8C,$AD,$72,$82,$3F,$67,$48,$24,$63,$87
	.byte	$DF,$60,$92,$C3,$59,$29,$E1,$29,$29,$77,$FC,$FB,$80,$5D,$5C,$7F
	.byte	$FA,$14,$6B,$90,$44,$03,$1A,$BA,$DF,$CF,$A5,$D7,$84,$80,$6E,$A0
	.byte	$FC,$F5,$05,$97,$1A,$AF,$EA,$F5,$6E,$00

qr147:	.byte	33
	.byte	$FE,$2F,$4D,$3F,$C1,$53,$E7,$10,$6E,$83,$47,$8B,$B7,$5D,$68,$75
	.byte	$DB,$A4,$1B,$72,$EC,$16,$C3,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$6D
	.byte	$00,$FB,$D0,$CA,$D5,$5E,$D7,$9E,$B0,$FF,$E9,$EB,$84,$15,$11,$A1
	.byte	$E6,$4A,$76,$B4,$2C,$81,$2C,$0F,$F9,$2C,$49,$60,$01,$55,$78,$25
	.byte	$5A,$B4,$92,$F0,$4B,$C9,$34,$E7,$BF,$F2,$C5,$E1,$C2,$A1,$5A,$25
	.byte	$94,$8C,$C4,$A4,$BE,$25,$96,$CF,$07,$EB,$1E,$ED,$E7,$18,$1D,$43
	.byte	$66,$6C,$FC,$BA,$F1,$4A,$FC,$80,$77,$4F,$45,$7F,$A1,$EA,$6B,$50
	.byte	$41,$90,$B1,$FB,$A8,$BD,$0F,$95,$D4,$04,$C0,$DE,$EA,$65,$8C,$91
	.byte	$05,$65,$4C,$CC,$FE,$D0,$6B,$B9,$00

qr148:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$08,$50,$6E,$9C,$CA,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$55,$48,$C1,$FC,$7A,$8B,$D0,$60,$4B,$30,$30,$20,$B6,$7D,$06,$56
	.byte	$E8,$59,$D5,$EA,$86,$BE,$8C,$95,$72,$82,$4E,$E7,$48,$24,$61,$A7
	.byte	$DF,$65,$D2,$C3,$59,$6B,$61,$29,$2A,$B3,$FC,$FB,$80,$5E,$5C,$7F
	.byte	$FB,$0C,$6B,$90,$45,$03,$1A,$BA,$9B,$CF,$A5,$D5,$B4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$17,$1A,$AF,$EB,$F5,$6E,$00

qr149:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$38,$50,$6E,$82,$CA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$DE,$55
	.byte	$56,$A1,$71,$FC,$60,$AA,$50,$60,$8B,$24,$30,$2A,$B6,$7D,$06,$1A
	.byte	$88,$59,$D4,$2C,$86,$BE,$9E,$B1,$72,$82,$DE,$EF,$48,$27,$63,$A7
	.byte	$DF,$60,$E9,$C3,$59,$52,$E1,$29,$2B,$B0,$5C,$FB,$80,$7A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$42,$03,$1A,$BA,$DF,$CF,$B5,$D7,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$37,$1A,$AF,$EE,$75,$6E,$00

qr150:	.byte	33
	.byte	$FE,$4C,$C3,$3F,$C1,$4F,$92,$D0,$6E,$BF,$32,$4B,$B7,$4E,$E7,$45
	.byte	$DB,$A8,$68,$32,$EC,$16,$B0,$85,$07,$FA,$AA,$AA,$FE,$01,$94,$AA
	.byte	$00,$D3,$21,$85,$BB,$5A,$19,$A6,$53,$50,$F8,$0E,$98,$6D,$30,$54
	.byte	$FA,$39,$E0,$85,$CF,$0A,$4B,$C3,$E5,$5E,$9D,$A1,$3D,$24,$F1,$DE
	.byte	$A9,$3A,$A7,$D6,$4F,$B8,$88,$00,$53,$83,$11,$B9,$19,$2F,$79,$2A
	.byte	$8A,$FD,$09,$B9,$A0,$54,$50,$49,$E4,$65,$27,$7B,$79,$49,$DA,$E0
	.byte	$B9,$6D,$3B,$BE,$93,$CC,$FC,$00,$5B,$4E,$C6,$7F,$BD,$B1,$AA,$50
	.byte	$4E,$08,$91,$0B,$A0,$CF,$4F,$95,$D7,$76,$07,$C2,$E9,$E9,$34,$73
	.byte	$05,$97,$8B,$D0,$FE,$E0,$0C,$A5,$00

qr151:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$E7,$10,$6E,$83,$47,$8B,$B7,$5D,$6B,$75
	.byte	$DB,$A4,$1A,$F2,$EC,$16,$C1,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$3D
	.byte	$00,$FB,$D0,$52,$D5,$48,$97,$8E,$B0,$F5,$A9,$DB,$84,$0C,$21,$97
	.byte	$E6,$42,$2E,$BD,$2C,$81,$E0,$04,$F9,$2F,$CB,$67,$A1,$54,$02,$66
	.byte	$4A,$B4,$BF,$D1,$5B,$C9,$5A,$E7,$7F,$F2,$DB,$D1,$EA,$A1,$5A,$3D
	.byte	$92,$8C,$C1,$B0,$BC,$25,$95,$46,$07,$EB,$1E,$3E,$E5,$38,$1D,$60
	.byte	$65,$3C,$FC,$BF,$70,$52,$FC,$80,$67,$0F,$45,$7F,$A1,$D8,$6B,$50
	.byte	$45,$86,$B1,$EB,$AA,$BE,$8F,$8D,$D5,$07,$C0,$DE,$EB,$67,$0C,$91
	.byte	$05,$E6,$4C,$CC,$FE,$B1,$CB,$B9,$00

qr152:	.byte	29
	.byte	$FE,$2A,$9B,$FC,$15,$18,$50,$6E,$82,$CA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$51,$6D,$51,$FC,$70,$8B,$50,$61,$C3,$24,$30,$2B,$B6,$7D,$06,$2E
	.byte	$A8,$59,$D6,$9A,$06,$BE,$80,$BD,$72,$82,$7E,$CF,$48,$27,$00,$97
	.byte	$DF,$66,$D1,$43,$59,$5B,$61,$29,$2A,$7E,$5C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4E,$03,$1A,$BA,$FF,$CF,$B5,$D5,$D4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EE,$75,$6E,$00

qr153:	.byte	33
	.byte	$FE,$2D,$1D,$3F,$C1,$53,$B7,$10,$6E,$83,$27,$8B,$B7,$5D,$70,$75
	.byte	$DB,$A4,$0D,$72,$EC,$16,$CE,$41,$07,$FA,$AA,$AA,$FE,$00,$E6,$1D
	.byte	$00,$FB,$D1,$42,$D5,$5A,$D7,$4E,$B0,$FC,$E9,$F9,$84,$0C,$31,$95
	.byte	$E6,$43,$36,$BD,$2C,$80,$82,$05,$F9,$2E,$1E,$64,$01,$54,$C2,$25
	.byte	$2A,$B4,$07,$D0,$43,$C9,$4E,$B7,$0F,$F2,$DA,$D9,$D0,$A1,$59,$9D
	.byte	$86,$8C,$C4,$3A,$BC,$25,$94,$23,$05,$EB,$1E,$5A,$E6,$38,$1D,$70
	.byte	$A6,$0C,$FC,$AF,$91,$D2,$FC,$80,$57,$7F,$45,$7F,$B9,$D2,$6B,$50
	.byte	$4D,$95,$B1,$FB,$A8,$A4,$8F,$8D,$D7,$01,$C0,$DE,$EA,$67,$2C,$91
	.byte	$05,$A7,$0C,$CC,$FE,$91,$4B,$B9,$00

qr154:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$03,$10,$6E,$9C,$AD,$8B,$B7,$5A,$18,$75
	.byte	$DB,$A2,$F3,$F2,$EC,$15,$3D,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$3D
	.byte	$00,$FB,$EB,$5A,$D5,$34,$20,$C6,$B0,$D6,$F6,$19,$84,$04,$8E,$52
	.byte	$E6,$46,$BD,$04,$AC,$82,$CB,$78,$F9,$2E,$4C,$1C,$21,$54,$28,$D9
	.byte	$5A,$B4,$CF,$AB,$C3,$C9,$5E,$B0,$5F,$F2,$DD,$AE,$60,$A1,$48,$2E
	.byte	$61,$8C,$CA,$FF,$14,$A5,$95,$80,$74,$EB,$1E,$8F,$9F,$B8,$1D,$53
	.byte	$D9,$6C,$FC,$A7,$AB,$EA,$FC,$80,$70,$7F,$45,$7F,$A6,$52,$6B,$50
	.byte	$4A,$54,$B1,$CB,$A9,$04,$0F,$8D,$D4,$70,$C0,$DE,$EB,$9C,$8C,$91
	.byte	$05,$98,$2C,$CC,$FE,$CB,$CB,$B9,$00

qr155:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$18,$50,$6E,$82,$8A,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6C,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$54,$41,$51,$FC,$71,$8A,$D0,$60,$51,$20,$30,$22,$3E,$7D,$06,$0E
	.byte	$0A,$59,$D6,$DB,$9E,$BE,$82,$11,$72,$82,$7F,$6F,$48,$26,$07,$A7
	.byte	$DF,$64,$91,$C3,$59,$13,$E1,$29,$28,$A6,$5C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$4A,$03,$1A,$BA,$9F,$CF,$B5,$D5,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$77,$1A,$AF,$EC,$75,$6E,$00

qr156:	.byte	33
	.byte	$FE,$42,$BD,$3F,$C1,$6F,$E7,$10,$6E,$9C,$4F,$8B,$B7,$5A,$22,$75
	.byte	$DB,$A2,$CD,$F2,$EC,$15,$3E,$41,$07,$FA,$AA,$AA,$FE,$00,$19,$3D
	.byte	$00,$FB,$EB,$DA,$D5,$66,$A0,$56,$B0,$ED,$96,$51,$84,$06,$BE,$53
	.byte	$E6,$4E,$B5,$05,$2C,$85,$65,$71,$F9,$2E,$4C,$9C,$81,$54,$BB,$D8
	.byte	$2A,$B4,$9B,$AB,$CB,$C9,$74,$60,$7F,$F2,$E8,$F6,$62,$A1,$5B,$22
	.byte	$65,$8C,$CB,$37,$1C,$A5,$97,$EC,$75,$EB,$1E,$BB,$9C,$98,$1D,$5A
	.byte	$D9,$1C,$FC,$9F,$2A,$4A,$FC,$80,$40,$2F,$45,$7F,$A6,$4A,$6B,$50
	.byte	$46,$55,$B1,$CB,$AD,$04,$0F,$9D,$D4,$77,$C0,$DE,$EA,$9E,$8C,$91
	.byte	$05,$5A,$2C,$CC,$FE,$8B,$4B,$B9,$00

qr157:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$58,$50,$6E,$84,$8A,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$56,$85,$81,$FC,$54,$88,$D0,$61,$0A,$24,$30,$20,$76,$7D,$06,$7C
	.byte	$E9,$59,$D6,$FF,$B6,$BE,$91,$02,$32,$82,$0A,$FF,$48,$24,$8D,$D7
	.byte	$DF,$64,$B3,$43,$59,$28,$B5,$29,$2B,$F6,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FB,$84,$6B,$90,$49,$43,$1B,$BA,$C7,$CF,$B5,$D7,$A4,$80,$6E,$BA
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$EF,$D5,$6E,$00

qr158:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$08,$50,$6E,$82,$8A,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$0F,$AE,$C1,$6E,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$51,$01,$41,$FC,$58,$EB,$50,$61,$31,$20,$30,$21,$A6,$7D,$06,$10
	.byte	$6A,$59,$D5,$0D,$86,$BE,$83,$09,$72,$82,$BE,$EF,$48,$27,$69,$97
	.byte	$DF,$6B,$D8,$C3,$59,$13,$61,$29,$2A,$A8,$5C,$FB,$80,$7A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$46,$03,$1A,$BA,$9F,$CF,$B5,$D5,$D4,$80,$6E,$AA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EA,$75,$6E,$00

qr159:	.byte	33
	.byte	$FE,$2F,$4D,$3F,$C1,$53,$E3,$10,$6E,$83,$47,$8B,$B7,$5D,$68,$75
	.byte	$DB,$A4,$19,$F2,$EC,$16,$C3,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$3D
	.byte	$00,$FB,$D0,$52,$D5,$36,$17,$9E,$B0,$C8,$C9,$CB,$84,$19,$21,$93
	.byte	$E6,$40,$EE,$BD,$AC,$87,$06,$04,$79,$2D,$6C,$E6,$21,$54,$38,$E6
	.byte	$6A,$B4,$D6,$31,$4B,$C9,$2C,$B7,$4F,$F2,$E5,$89,$E2,$A1,$58,$91
	.byte	$93,$8C,$C9,$E4,$BC,$25,$97,$0A,$04,$EB,$1E,$6C,$65,$98,$1D,$03
	.byte	$65,$7C,$FC,$A7,$50,$52,$FC,$80,$47,$3F,$45,$7F,$A1,$D2,$6B,$50
	.byte	$49,$84,$B1,$EB,$A8,$BF,$0F,$8D,$D5,$07,$C0,$DE,$EB,$67,$0C,$91
	.byte	$05,$E6,$6C,$CC,$FE,$D1,$CB,$B9,$00

qr160:	.byte	33
	.byte	$FE,$99,$96,$3F,$C1,$77,$71,$50,$6E,$8E,$F1,$4B,$B7,$44,$46,$E5
	.byte	$DB,$AF,$76,$42,$EC,$14,$88,$65,$07,$FA,$AA,$AA,$FE,$01,$77,$B4
	.byte	$00,$E6,$E6,$A9,$F9,$F6,$B3,$1C,$F9,$C7,$C4,$6F,$5F,$71,$28,$86
	.byte	$74,$05,$A5,$DB,$1A,$5D,$26,$4C,$DD,$BF,$CE,$3C,$EC,$E3,$0A,$77
	.byte	$53,$90,$92,$27,$70,$A4,$92,$E3,$0D,$BB,$DF,$BC,$6C,$7A,$34,$3C
	.byte	$BF,$1E,$8B,$29,$D6,$93,$49,$41,$4C,$CF,$8F,$49,$BC,$F5,$AA,$49
	.byte	$77,$65,$D8,$DB,$87,$D9,$F9,$00,$43,$CD,$44,$7F,$9C,$2C,$AA,$30
	.byte	$54,$AB,$31,$8B,$A7,$D8,$BF,$C5,$D2,$48,$E4,$46,$EA,$BC,$61,$27
	.byte	$05,$77,$65,$E8,$FE,$87,$70,$D4,$80

qr161:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$27,$10,$6E,$9C,$A5,$8B,$B7,$5A,$18,$75
	.byte	$DB,$A2,$F7,$F2,$EC,$15,$3C,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$3D
	.byte	$00,$FB,$EB,$5A,$D5,$0E,$60,$F6,$B0,$D1,$D6,$01,$84,$0E,$8E,$51
	.byte	$E6,$45,$3D,$04,$AC,$86,$43,$79,$F9,$2F,$1E,$9C,$A1,$54,$89,$99
	.byte	$1A,$B4,$8A,$4B,$C3,$C9,$4E,$60,$7F,$F2,$EA,$8E,$72,$A1,$5A,$92
	.byte	$66,$8C,$CA,$FB,$15,$25,$94,$C2,$75,$EB,$1E,$7A,$9E,$38,$1D,$53
	.byte	$D9,$6C,$FC,$A6,$4B,$E2,$FC,$80,$60,$7F,$45,$7F,$AE,$52,$6B,$50
	.byte	$4A,$57,$B1,$CB,$AB,$05,$0F,$8D,$D6,$70,$C0,$DE,$EA,$9C,$8C,$91
	.byte	$05,$D8,$2C,$CC,$FE,$AB,$CB,$B9,$00

qr162:	.byte	29
	.byte	$FE,$98,$43,$FC,$13,$34,$90,$6E,$8B,$AE,$BB,$74,$18,$D5,$DB,$A5
	.byte	$4B,$2E,$C1,$16,$01,$07,$FA,$AA,$AF,$E0,$16,$F0,$00,$DA,$7A,$C2
	.byte	$0A,$EE,$6D,$8D,$B0,$AE,$C2,$28,$1A,$C7,$BE,$19,$35,$10,$B0,$8E
	.byte	$4B,$D7,$EF,$8C,$0C,$F7,$A9,$3D,$03,$45,$63,$9D,$01,$04,$C1,$39
	.byte	$E7,$B4,$B5,$F5,$83,$F8,$EB,$11,$CD,$6A,$15,$FF,$00,$6B,$9C,$63
	.byte	$F8,$F5,$6B,$10,$40,$3B,$19,$BA,$A9,$1F,$DD,$D4,$EC,$63,$EE,$93
	.byte	$58,$67,$05,$30,$06,$DF,$EB,$51,$FC,$00

qr163:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$4C,$50,$6E,$84,$8A,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$62,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CE,$55
	.byte	$55,$41,$A1,$FC,$4F,$A9,$D0,$60,$89,$24,$30,$2A,$F6,$7D,$06,$2E
	.byte	$89,$59,$D4,$5C,$AE,$BE,$8D,$8A,$32,$82,$AF,$7F,$48,$25,$A0,$C7
	.byte	$DF,$6B,$92,$43,$59,$01,$B5,$29,$2A,$E8,$7C,$FB,$80,$78,$5C,$7F
	.byte	$FB,$9C,$6B,$90,$45,$43,$1B,$BA,$A7,$CF,$B5,$D5,$84,$80,$6E,$AA
	.byte	$FC,$F5,$05,$27,$1A,$AF,$E9,$D5,$6E,$00

qr164:	.byte	33
	.byte	$FE,$2C,$4D,$3F,$C1,$53,$A3,$10,$6E,$82,$03,$8B,$B7,$5D,$F9,$75
	.byte	$DB,$A4,$2B,$F2,$EC,$16,$FD,$41,$07,$FA,$AA,$AA,$FE,$00,$E6,$1D
	.byte	$00,$FB,$D7,$CA,$D5,$4C,$D4,$4E,$B0,$D7,$E8,$43,$84,$15,$91,$57
	.byte	$E6,$4A,$F6,$E1,$2C,$87,$46,$12,$F9,$2F,$0A,$FF,$01,$55,$59,$A2
	.byte	$1A,$B4,$FB,$10,$CB,$C9,$2A,$76,$7F,$F2,$CF,$99,$D2,$A1,$42,$21
	.byte	$A3,$8C,$C0,$7A,$AD,$25,$94,$A7,$07,$EB,$1E,$1F,$67,$B8,$1D,$03
	.byte	$66,$3C,$FC,$82,$D1,$12,$FC,$80,$47,$6F,$45,$7F,$A9,$D0,$6B,$50
	.byte	$41,$B0,$B1,$FB,$A8,$A5,$0F,$9D,$D6,$05,$C0,$DE,$EA,$E7,$2C,$91
	.byte	$05,$66,$2C,$CC,$FE,$91,$4B,$B9,$00

qr165:	.byte	33
	.byte	$FE,$86,$81,$3F,$C1,$4F,$8D,$10,$6E,$B2,$93,$6B,$B7,$5D,$05,$05
	.byte	$DB,$A6,$82,$32,$EC,$17,$05,$A1,$07,$FA,$AA,$AA,$FE,$00,$6B,$CA
	.byte	$00,$CE,$2C,$5E,$97,$BA,$83,$50,$88,$2A,$F8,$23,$67,$97,$B9,$4F
	.byte	$97,$81,$31,$7C,$6B,$99,$8D,$43,$9A,$A3,$5E,$7C,$2F,$6C,$E3,$6B
	.byte	$ED,$A8,$8B,$8C,$6F,$B8,$CE,$63,$71,$CA,$06,$A8,$2A,$42,$C9,$B9
	.byte	$4A,$FD,$02,$BB,$75,$E2,$8F,$88,$40,$08,$90,$8B,$FF,$36,$24,$49
	.byte	$68,$CB,$E0,$EA,$2C,$DE,$FD,$00,$63,$C1,$45,$BF,$98,$40,$AA,$D0
	.byte	$55,$7B,$D1,$1B,$AD,$66,$CF,$8D,$D1,$4F,$23,$52,$E9,$7C,$A2,$A9
	.byte	$05,$A8,$AB,$D0,$FE,$AC,$D7,$C8,$80

qr166:	.byte	29
	.byte	$FE,$0C,$9B,$FC,$15,$7C,$50,$6E,$84,$0A,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$61,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$50,$81,$81,$FC,$4A,$88,$50,$61,$22,$24,$30,$27,$76,$7D,$06,$24
	.byte	$AA,$59,$D7,$5C,$B6,$BE,$9F,$96,$72,$82,$76,$3F,$48,$26,$06,$C7
	.byte	$DF,$69,$FA,$C3,$59,$30,$B5,$29,$2B,$20,$7C,$FB,$80,$59,$5C,$7F
	.byte	$FB,$14,$6B,$90,$41,$03,$1B,$BA,$A7,$CF,$B5,$D7,$94,$80,$6E,$A3
	.byte	$7C,$F5,$05,$E7,$1A,$AF,$EB,$D5,$6E,$00

qr167:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$4C,$50,$6E,$9C,$0A,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$8F,$AE,$C1,$64,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$51,$A3,$81,$FC,$40,$E9,$50,$60,$43,$94,$30,$23,$F4,$7D,$06,$1A
	.byte	$E8,$59,$D7,$CD,$2E,$BE,$89,$8E,$72,$82,$8B,$DB,$48,$24,$60,$F7
	.byte	$DF,$65,$CB,$C3,$59,$18,$05,$29,$28,$F2,$7C,$FB,$80,$48,$5C,$7F
	.byte	$FB,$94,$6B,$90,$48,$43,$18,$BA,$8F,$CF,$A5,$D6,$84,$80,$6E,$BA
	.byte	$7C,$F5,$05,$47,$1A,$AF,$EC,$F5,$6E,$00

qr168:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$0C,$50,$6E,$9C,$0A,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$57,$E8,$C1,$FC,$42,$8B,$50,$61,$91,$34,$30,$25,$AE,$7D,$06,$26
	.byte	$88,$59,$D4,$5D,$8E,$BE,$95,$85,$72,$82,$2E,$E3,$48,$25,$63,$B7
	.byte	$DF,$63,$C3,$C3,$59,$6A,$A5,$29,$2B,$31,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FA,$8C,$6B,$90,$47,$43,$1A,$BA,$C7,$CF,$A5,$D4,$84,$80,$6E,$B9
	.byte	$FC,$F5,$05,$27,$1A,$AF,$E8,$F5,$6E,$00

qr169:	.byte	33
	.byte	$FE,$41,$BD,$3F,$C1,$6C,$17,$10,$6E,$9C,$A3,$8B,$B7,$5A,$19,$75
	.byte	$DB,$A2,$F1,$F2,$EC,$15,$3F,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$4D
	.byte	$00,$FB,$EB,$4A,$D5,$56,$20,$FE,$B0,$EC,$96,$1B,$84,$1E,$BE,$53
	.byte	$E6,$48,$E5,$0D,$AC,$86,$CF,$78,$79,$2E,$29,$9F,$A1,$54,$0A,$1A
	.byte	$1A,$B4,$4B,$0B,$63,$C9,$6C,$70,$FF,$F2,$E9,$86,$12,$A1,$41,$3A
	.byte	$56,$8C,$C8,$E3,$04,$25,$96,$AC,$78,$EB,$1E,$2B,$9C,$B8,$1D,$19
	.byte	$99,$0C,$FC,$AF,$CB,$C2,$FC,$80,$60,$4F,$45,$7F,$AE,$78,$6B,$50
	.byte	$4E,$64,$B1,$DB,$AB,$16,$8F,$95,$D4,$77,$C0,$DE,$EA,$9F,$2C,$91
	.byte	$05,$59,$6C,$CC,$FE,$8B,$CB,$B9,$00

qr170:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$F7,$10,$6E,$83,$49,$8B,$B7,$5D,$68,$75
	.byte	$DB,$A4,$19,$F2,$EC,$16,$C3,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$7D
	.byte	$00,$FB,$D0,$CA,$D5,$10,$17,$BE,$B0,$FF,$E9,$EB,$84,$1E,$31,$A3
	.byte	$E6,$4B,$BE,$B5,$AC,$86,$08,$0F,$79,$2F,$CA,$61,$21,$54,$F3,$A5
	.byte	$1A,$B4,$9E,$70,$43,$C9,$68,$A7,$8F,$F2,$EF,$B9,$DA,$A1,$50,$85
	.byte	$96,$8C,$C2,$B6,$BE,$25,$96,$88,$07,$EB,$1E,$5E,$67,$38,$1D,$18
	.byte	$66,$4C,$FC,$A2,$D1,$42,$FC,$80,$77,$4F,$45,$7F,$A1,$E8,$6B,$50
	.byte	$41,$90,$B1,$FB,$A8,$BC,$8F,$95,$D4,$04,$C0,$DE,$EB,$65,$AC,$91
	.byte	$05,$65,$6C,$CC,$FE,$D0,$6B,$B9,$00

qr171:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$F7,$10,$6E,$83,$47,$8B,$B7,$5D,$6B,$75
	.byte	$DB,$A4,$1A,$F2,$EC,$16,$C1,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$1D
	.byte	$00,$FB,$D0,$52,$D5,$14,$57,$BE,$B0,$FD,$C9,$C3,$84,$05,$91,$90
	.byte	$E6,$49,$BE,$BC,$2C,$81,$0A,$04,$F9,$2C,$FD,$66,$A1,$54,$7B,$A6
	.byte	$4A,$B4,$CA,$11,$53,$C9,$74,$C7,$7F,$F2,$C8,$B9,$E2,$A1,$4A,$81
	.byte	$96,$8C,$CF,$66,$BC,$25,$96,$4F,$05,$EB,$1E,$D9,$E4,$38,$1D,$63
	.byte	$A5,$1C,$FC,$86,$B0,$42,$FC,$80,$67,$0F,$45,$7F,$A9,$D8,$6B,$50
	.byte	$4D,$84,$B1,$EB,$A8,$BE,$8F,$8D,$D5,$07,$C0,$DE,$EA,$E7,$2C,$91
	.byte	$05,$26,$6C,$CC,$FE,$F1,$CB,$B9,$00

qr172:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$CC,$50,$6E,$9C,$0A,$BB,$75,$BF,$C5,$DB,$A2
	.byte	$8F,$AE,$C1,$55,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$51,$0A,$81,$FC,$6C,$D1,$50,$60,$DB,$D4,$30,$22,$3C,$FD,$06,$06
	.byte	$39,$59,$D5,$8C,$0E,$BE,$80,$B2,$32,$82,$8B,$C7,$48,$26,$A8,$97
	.byte	$DF,$62,$F3,$43,$59,$6A,$61,$29,$29,$F9,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FB,$AC,$6B,$90,$43,$43,$18,$BA,$C5,$CF,$B5,$D5,$A4,$80,$6E,$A1
	.byte	$FC,$F5,$05,$53,$1A,$AF,$E8,$75,$6E,$00

qr173:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$0C,$50,$6E,$9C,$0A,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$6F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DA,$55
	.byte	$51,$0C,$F1,$FC,$6B,$CB,$D0,$61,$00,$34,$30,$26,$BE,$7D,$06,$3A
	.byte	$0A,$59,$D6,$78,$86,$BE,$95,$A1,$32,$82,$1B,$67,$48,$25,$6F,$97
	.byte	$DF,$68,$A2,$C3,$59,$63,$65,$29,$29,$F3,$FC,$FB,$80,$5E,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4F,$03,$1A,$BA,$C7,$CF,$A5,$D6,$94,$80,$6E,$B0
	.byte	$FC,$F5,$05,$43,$1A,$AF,$EA,$F5,$6E,$00

qr174:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$0F,$B9,$10,$6E,$95,$85,$0B,$B7,$46,$7B,$B5
	.byte	$DB,$A3,$D7,$62,$EC,$12,$B6,$39,$07,$FA,$AA,$AA,$FE,$01,$97,$15
	.byte	$00,$DA,$4F,$58,$A0,$CA,$51,$89,$AC,$98,$B2,$EB,$CD,$34,$80,$59
	.byte	$05,$CE,$6E,$7D,$1A,$59,$A7,$4E,$1A,$A0,$C8,$8D,$68,$70,$A8,$1E
	.byte	$36,$C5,$77,$79,$82,$ED,$A8,$28,$BC,$7C,$D3,$EB,$EC,$7A,$3B,$AA
	.byte	$B4,$02,$F4,$A7,$4C,$81,$04,$07,$6D,$1A,$DA,$AF,$D5,$9C,$8F,$62
	.byte	$7B,$82,$C4,$DA,$3D,$01,$F9,$00,$73,$D1,$45,$BF,$8F,$5C,$EB,$10
	.byte	$42,$11,$71,$CB,$AE,$38,$1F,$C5,$D7,$F9,$F8,$3E,$E9,$46,$C1,$27
	.byte	$05,$57,$14,$2F,$FE,$CE,$F9,$F0,$00

qr175:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$07,$10,$6E,$9C,$A3,$8B,$B7,$5A,$18,$75
	.byte	$DB,$A2,$F7,$F2,$EC,$15,$3D,$01,$07,$FA,$AA,$AA,$FE,$00,$1A,$1D
	.byte	$00,$FB,$EB,$42,$D5,$52,$A0,$CE,$B0,$EC,$D6,$1B,$84,$00,$1E,$53
	.byte	$E6,$48,$6D,$0D,$AC,$82,$CB,$79,$79,$2F,$0E,$1E,$21,$54,$C0,$1A
	.byte	$2A,$B4,$DA,$CB,$6B,$C9,$06,$40,$FF,$F2,$C5,$E6,$12,$A1,$58,$9E
	.byte	$57,$8C,$C5,$63,$04,$25,$95,$80,$78,$AB,$1E,$AE,$9C,$B8,$1D,$12
	.byte	$59,$0C,$FC,$AB,$EB,$DA,$FC,$80,$40,$4F,$45,$7F,$A6,$78,$6B,$50
	.byte	$4A,$64,$B1,$DB,$AB,$16,$8F,$95,$D4,$77,$C0,$DE,$EB,$9F,$2C,$91
	.byte	$05,$19,$6C,$CC,$FE,$AB,$CB,$B9,$00

qr176:	.byte	33
	.byte	$FE,$22,$73,$3F,$C1,$22,$1F,$D0,$6E,$A4,$48,$0B,$B7,$49,$96,$45
	.byte	$DB,$A4,$CB,$12,$EC,$11,$DD,$8D,$07,$FA,$AA,$AA,$FE,$01,$22,$DE
	.byte	$00,$EF,$88,$C4,$E2,$4A,$AE,$E6,$53,$66,$AE,$E0,$0A,$29,$3D,$DE
	.byte	$DE,$A8,$23,$34,$CF,$0A,$A1,$9B,$77,$15,$CD,$11,$99,$B6,$0B,$62
	.byte	$99,$3A,$8B,$28,$F5,$F1,$CC,$1E,$CF,$11,$4B,$96,$F3,$2F,$66,$31
	.byte	$D8,$B4,$2C,$65,$3C,$46,$1B,$A6,$9B,$65,$26,$ED,$12,$80,$FE,$89
	.byte	$21,$EF,$72,$92,$C8,$4C,$FC,$00,$6E,$77,$C6,$FF,$AE,$9B,$EB,$70
	.byte	$51,$EB,$91,$3B,$AF,$2E,$6F,$9D,$D2,$94,$4E,$E6,$EA,$11,$14,$73
	.byte	$05,$A1,$8F,$42,$FE,$88,$45,$81,$80

qr177:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$5C,$50,$6E,$9C,$0A,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$8F,$AE,$C1,$64,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$51,$AF,$91,$FC,$5F,$C9,$50,$61,$28,$90,$30,$2E,$AC,$3D,$06,$7C
	.byte	$AB,$59,$D6,$DE,$3E,$BE,$92,$AE,$72,$82,$8A,$BF,$48,$24,$68,$C7
	.byte	$DF,$6D,$BB,$43,$59,$0B,$71,$29,$28,$70,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$8C,$6B,$90,$40,$03,$18,$BA,$8F,$CF,$A5,$D4,$84,$80,$6E,$A2
	.byte	$7C,$F5,$05,$77,$1A,$AF,$E9,$75,$6E,$00

qr178:	.byte	33
	.byte	$FE,$86,$E1,$3F,$C1,$4F,$99,$10,$6E,$B2,$9B,$6B,$B7,$5D,$05,$05
	.byte	$DB,$A6,$80,$32,$EC,$17,$07,$E1,$07,$FA,$AA,$AA,$FE,$00,$6B,$EA
	.byte	$00,$CE,$2C,$56,$97,$9E,$43,$50,$88,$05,$B8,$3B,$67,$98,$A9,$4E
	.byte	$97,$8E,$29,$7D,$EB,$9E,$29,$40,$1A,$A1,$4E,$7C,$2F,$6D,$F8,$6B
	.byte	$BD,$A8,$9E,$6C,$67,$B8,$AA,$A3,$79,$CA,$19,$A8,$28,$42,$C6,$B1
	.byte	$4A,$FD,$0D,$75,$75,$62,$8E,$84,$40,$08,$90,$DF,$7F,$36,$24,$7A
	.byte	$E8,$CB,$E0,$EF,$6C,$DE,$FD,$00,$43,$C1,$45,$BF,$88,$40,$AA,$D0
	.byte	$59,$79,$D1,$1B,$AF,$67,$4F,$8D,$D1,$4F,$23,$52,$E8,$7C,$A2,$A9
	.byte	$05,$A8,$AB,$D0,$FE,$CC,$D7,$C8,$80

qr179:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$1C,$50,$6E,$9C,$0A,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$57,$44,$D1,$FC,$59,$EB,$50,$60,$2B,$30,$30,$20,$76,$3D,$06,$50
	.byte	$8B,$59,$D4,$C9,$1E,$BE,$9E,$35,$72,$82,$2B,$E7,$48,$25,$E4,$97
	.byte	$DF,$6E,$C2,$C3,$59,$28,$21,$29,$28,$B3,$FC,$FB,$80,$4E,$5C,$7F
	.byte	$FB,$24,$6B,$90,$46,$03,$1A,$BA,$EF,$CF,$A5,$D5,$A4,$80,$6E,$B0
	.byte	$7C,$F5,$05,$87,$1A,$AF,$EA,$F5,$6E,$00

qr180:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$1C,$50,$6E,$92,$0A,$BB,$75,$9F,$C5,$DB,$A1
	.byte	$0F,$AE,$C1,$50,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$56,$46,$91,$FC,$49,$B3,$50,$61,$F1,$E0,$30,$23,$35,$BD,$06,$36
	.byte	$1F,$59,$D5,$A9,$BE,$BE,$9A,$83,$72,$82,$4E,$07,$48,$27,$29,$97
	.byte	$DF,$69,$88,$C3,$59,$22,$51,$29,$28,$7A,$7C,$FB,$80,$49,$5C,$7F
	.byte	$FB,$B4,$6B,$90,$4A,$03,$19,$BA,$DF,$CF,$B5,$D6,$C4,$80,$6E,$A8
	.byte	$FC,$F5,$05,$77,$1A,$AF,$E9,$F5,$6E,$00

qr181:	.byte	33
	.byte	$FE,$86,$81,$3F,$C1,$4F,$BD,$10,$6E,$B2,$9D,$6B,$B7,$5D,$1D,$05
	.byte	$DB,$A6,$84,$32,$EC,$17,$02,$A1,$07,$FA,$AA,$AA,$FE,$00,$68,$CA
	.byte	$00,$CE,$2C,$DE,$97,$AC,$43,$C0,$88,$23,$D8,$53,$67,$80,$A9,$7B
	.byte	$97,$8B,$71,$65,$6B,$9B,$E1,$4D,$1A,$A3,$7E,$FD,$2F,$6C,$71,$68
	.byte	$FD,$A8,$4A,$4C,$C7,$B8,$C8,$93,$C9,$CA,$08,$F8,$60,$42,$D9,$89
	.byte	$4A,$FD,$07,$F7,$75,$62,$8E,$29,$48,$08,$90,$28,$7F,$B6,$24,$4B
	.byte	$29,$8B,$E0,$C6,$8C,$9E,$FD,$00,$73,$E1,$45,$BF,$98,$48,$AA,$D0
	.byte	$51,$7D,$D1,$1B,$AF,$6F,$4F,$95,$D2,$4C,$23,$52,$E9,$FE,$22,$A9
	.byte	$05,$A8,$AB,$D0,$FE,$AD,$77,$C8,$80

qr182:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$5C,$50,$6E,$84,$0A,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$0F,$AE,$C1,$60,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$52,$05,$91,$FC,$65,$A9,$50,$60,$F9,$20,$30,$2D,$FE,$7D,$06,$4E
	.byte	$AA,$59,$D4,$9C,$AE,$BE,$86,$02,$72,$82,$D7,$FF,$48,$24,$4C,$C7
	.byte	$DF,$6A,$8A,$43,$59,$08,$35,$29,$29,$70,$7C,$FB,$80,$6B,$5C,$7F
	.byte	$FB,$14,$6B,$90,$41,$03,$1B,$BA,$A7,$CF,$B5,$D5,$84,$80,$6E,$B2
	.byte	$FC,$F5,$05,$27,$1A,$AF,$EF,$D5,$6E,$00

qr183:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$1C,$50,$6E,$92,$0A,$BB,$75,$9F,$C5,$DB,$A1
	.byte	$0F,$AE,$C1,$50,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$55,$E6,$91,$FC,$74,$F3,$50,$61,$41,$E0,$30,$26,$6D,$FD,$06,$0C
	.byte	$1E,$59,$D7,$79,$AE,$BE,$93,$2F,$72,$82,$AA,$47,$48,$26,$49,$A7
	.byte	$DF,$6D,$88,$43,$59,$68,$15,$29,$29,$B0,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FB,$3C,$6B,$90,$4A,$03,$19,$BA,$BF,$CF,$B5,$D5,$C4,$80,$6E,$A0
	.byte	$FC,$F5,$05,$77,$1A,$AF,$EA,$75,$6E,$00

qr184:	.byte	33
	.byte	$FE,$19,$A6,$3F,$C1,$33,$55,$10,$6E,$83,$47,$8B,$B7,$51,$1A,$B5
	.byte	$DB,$AC,$1E,$F2,$EC,$10,$CA,$21,$07,$FA,$AA,$AA,$FE,$00,$67,$65
	.byte	$00,$C7,$50,$4A,$8C,$6E,$A6,$49,$AC,$B4,$E9,$D3,$84,$14,$29,$B4
	.byte	$64,$42,$ED,$D1,$9A,$59,$E8,$0D,$D9,$AD,$CD,$E6,$81,$54,$F8,$21
	.byte	$06,$C5,$13,$51,$53,$C9,$28,$77,$DD,$FA,$E8,$F4,$56,$7A,$31,$B9
	.byte	$B5,$0E,$C7,$F6,$BD,$25,$96,$C1,$19,$9A,$DA,$4A,$64,$38,$1D,$59
	.byte	$27,$14,$DC,$D2,$66,$99,$F9,$00,$67,$8D,$45,$7F,$A1,$D8,$6B,$50
	.byte	$5D,$F4,$71,$FB,$A2,$BE,$8F,$8D,$D0,$0F,$E0,$5E,$E8,$3C,$41,$27
	.byte	$05,$A4,$64,$EC,$FE,$F1,$CB,$B9,$00

qr185:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$5C,$50,$6E,$9C,$0A,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$AF,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$54,$0F,$A1,$FC,$76,$89,$D0,$60,$0A,$90,$30,$2D,$24,$7D,$06,$62
	.byte	$E9,$59,$D6,$0B,$AE,$BE,$88,$12,$32,$82,$4A,$7B,$48,$26,$07,$F7
	.byte	$DF,$60,$81,$C3,$59,$78,$45,$29,$2B,$30,$FC,$FB,$80,$48,$5C,$7F
	.byte	$FA,$34,$6B,$90,$44,$43,$18,$BA,$BF,$CF,$A5,$D5,$B4,$80,$6E,$B0
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EA,$F5,$6E,$00

qr186:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$1C,$50,$6E,$9C,$8A,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$0F,$AE,$C1,$56,$E1,$07,$FA,$AA,$AF,$E0,$0A,$53,$00,$FB,$DE,$55
	.byte	$52,$AC,$C1,$FC,$6F,$AB,$50,$60,$1A,$34,$30,$23,$EE,$7D,$06,$58
	.byte	$08,$59,$D6,$3B,$8E,$BE,$8F,$09,$72,$82,$A6,$A7,$48,$24,$02,$97
	.byte	$DF,$6B,$AB,$43,$59,$10,$E5,$29,$2B,$7F,$FC,$FB,$80,$6D,$5C,$7F
	.byte	$FB,$34,$6B,$90,$46,$43,$1A,$BA,$BF,$CF,$A5,$D6,$A4,$80,$6E,$B0
	.byte	$7C,$F5,$05,$27,$1A,$AF,$EB,$F5,$6E,$00

qr187:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$DC,$50,$6E,$9C,$8A,$BB,$75,$BF,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$02,$53,$00,$FB,$E6,$55
	.byte	$54,$42,$81,$FC,$4E,$D1,$50,$60,$D0,$D4,$30,$2C,$EC,$FD,$06,$74
	.byte	$3A,$59,$D6,$8B,$86,$BE,$83,$2A,$72,$82,$17,$47,$48,$27,$C7,$87
	.byte	$DF,$68,$DA,$C3,$59,$49,$A1,$29,$2B,$BD,$FC,$FB,$80,$6C,$5C,$7F
	.byte	$FA,$2C,$6B,$90,$47,$43,$18,$BA,$A5,$CF,$B5,$D6,$A4,$80,$6E,$B9
	.byte	$FC,$F5,$05,$D3,$1A,$AF,$EC,$75,$6E,$00

qr188:	.byte	29
	.byte	$FE,$52,$9B,$FC,$16,$1C,$50,$6E,$9C,$8A,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$2F,$AE,$C1,$54,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$54,$C0,$C1,$FC,$56,$8A,$D0,$60,$EB,$34,$30,$25,$EE,$7D,$06,$38
	.byte	$C9,$59,$D6,$68,$16,$BE,$91,$81,$32,$82,$AF,$23,$48,$27,$CB,$B7
	.byte	$DF,$66,$9B,$C3,$59,$1A,$A5,$29,$2B,$F1,$FC,$FB,$80,$7E,$5C,$7F
	.byte	$FB,$84,$6B,$90,$41,$03,$1A,$BA,$B7,$CF,$A5,$D7,$94,$80,$6E,$B0
	.byte	$FC,$F5,$05,$C7,$1A,$AF,$E8,$F5,$6E,$00

qr189:	.byte	33
	.byte	$FE,$2F,$1D,$3F,$C1,$53,$E3,$10,$6E,$83,$4F,$8B,$B7,$5D,$6A,$75
	.byte	$DB,$A4,$1B,$F2,$EC,$16,$C1,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$0D
	.byte	$00,$FB,$D0,$D2,$D5,$2C,$57,$9E,$B0,$D7,$A9,$E3,$84,$1E,$11,$A7
	.byte	$E6,$42,$2E,$B5,$AC,$83,$A0,$0D,$79,$2E,$DB,$E0,$01,$54,$FB,$A5
	.byte	$2A,$B4,$4A,$B0,$4B,$C9,$3A,$77,$8F,$F2,$FD,$B1,$DA,$A1,$5D,$85
	.byte	$95,$8C,$CF,$60,$BE,$25,$96,$E4,$07,$EB,$1E,$2A,$67,$18,$1D,$73
	.byte	$A6,$4C,$FC,$AE,$31,$4A,$FC,$80,$47,$4F,$45,$7F,$B1,$EA,$6B,$50
	.byte	$45,$91,$B1,$FB,$AA,$BC,$8F,$95,$D7,$04,$C0,$DE,$EA,$E5,$AC,$91
	.byte	$05,$E5,$6C,$CC,$FE,$B0,$6B,$B9,$00

qr190:	.byte	33
	.byte	$FE,$2F,$7D,$3F,$C1,$53,$D3,$10,$6E,$83,$4B,$8B,$B7,$5D,$69,$75
	.byte	$DB,$A4,$1B,$F2,$EC,$16,$C0,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$3D
	.byte	$00,$FB,$D0,$D2,$D5,$4C,$D7,$BE,$B0,$E3,$C9,$E3,$84,$0B,$21,$A7
	.byte	$E6,$42,$3E,$B5,$AC,$84,$0E,$0F,$79,$2C,$BF,$60,$01,$55,$AA,$65
	.byte	$6A,$B4,$EE,$10,$4B,$C9,$3A,$87,$BF,$F2,$FF,$A1,$D2,$A1,$44,$85
	.byte	$91,$8C,$C5,$FC,$BE,$25,$94,$40,$07,$EB,$1E,$0C,$E7,$18,$1D,$7A
	.byte	$66,$4C,$FC,$8E,$51,$4A,$FC,$80,$67,$4F,$45,$7F,$B1,$EA,$6B,$50
	.byte	$45,$91,$B1,$FB,$AC,$BC,$8F,$95,$D4,$04,$C0,$DE,$EA,$E5,$AC,$91
	.byte	$05,$65,$6C,$CC,$FE,$90,$6B,$B9,$00

qr191:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$F7,$10,$6E,$83,$43,$8B,$B7,$5D,$69,$75
	.byte	$DB,$A4,$1F,$F2,$EC,$16,$C3,$01,$07,$FA,$AA,$AA,$FE,$00,$E5,$0D
	.byte	$00,$FB,$D0,$CA,$D5,$72,$17,$BE,$B0,$DE,$89,$E3,$84,$1C,$01,$A3
	.byte	$E6,$4F,$2E,$B5,$AC,$85,$48,$0D,$79,$2E,$D9,$60,$81,$55,$11,$A5
	.byte	$2A,$B4,$67,$30,$4B,$C9,$4A,$87,$BF,$F2,$D5,$81,$D2,$A1,$4E,$3D
	.byte	$95,$8C,$C1,$24,$BE,$25,$96,$A3,$07,$EB,$1E,$4F,$67,$18,$1D,$10
	.byte	$26,$4C,$FC,$B2,$71,$4A,$FC,$80,$67,$4F,$45,$7F,$A1,$EA,$6B,$50
	.byte	$49,$91,$B1,$FB,$AE,$BC,$8F,$95,$D4,$04,$C0,$DE,$EA,$E5,$AC,$91
	.byte	$05,$65,$6C,$CC,$FE,$F0,$6B,$B9,$00

qr192:	.byte	33
	.byte	$FE,$41,$BD,$3F,$C1,$6C,$33,$10,$6E,$9C,$A3,$8B,$B7,$5A,$1A,$75
	.byte	$DB,$A2,$F7,$F2,$EC,$15,$3E,$41,$07,$FA,$AA,$AA,$FE,$00,$1A,$0D
	.byte	$00,$FB,$EB,$42,$D5,$46,$A0,$FE,$B0,$C7,$F6,$11,$84,$19,$2E,$57
	.byte	$E6,$4D,$ED,$0C,$2C,$85,$05,$78,$F9,$2E,$FB,$1F,$81,$55,$B2,$5A
	.byte	$2A,$B4,$5E,$CB,$6B,$C9,$3A,$90,$FF,$F2,$F0,$96,$12,$A1,$48,$B6
	.byte	$55,$8C,$C8,$AF,$04,$25,$97,$4D,$78,$EB,$1E,$CC,$1C,$98,$1D,$69
	.byte	$D9,$0C,$FC,$B2,$2B,$CA,$FC,$80,$40,$4F,$45,$7F,$BE,$7A,$6B,$50
	.byte	$4E,$65,$B1,$DB,$AB,$16,$8F,$95,$D5,$77,$C0,$DE,$EB,$1F,$2C,$91
	.byte	$05,$59,$6C,$CC,$FE,$8B,$CB,$B9,$00

qr193:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$48,$A5,$50,$6E,$91,$13,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9A,$42,$EC,$17,$75,$65,$07,$FA,$AA,$AA,$FE,$01,$88,$14
	.byte	$00,$E6,$DD,$91,$F9,$A2,$84,$4C,$F9,$EC,$BB,$B7,$5F,$74,$97,$76
	.byte	$74,$0E,$36,$61,$9A,$5E,$4B,$31,$DD,$BC,$CE,$44,$EC,$E3,$D0,$08
	.byte	$03,$90,$12,$9D,$A8,$A4,$FE,$84,$6D,$BB,$C9,$BB,$A4,$7A,$27,$27
	.byte	$72,$1E,$8E,$6C,$69,$13,$4B,$05,$31,$CF,$8F,$E9,$47,$D5,$AA,$5B
	.byte	$4B,$45,$D8,$F7,$BD,$19,$F9,$00,$54,$DD,$44,$7F,$9B,$CC,$AA,$30
	.byte	$5B,$43,$31,$9B,$A2,$7B,$3F,$CD,$D2,$3E,$E4,$4E,$EB,$44,$41,$27
	.byte	$05,$CB,$25,$E8,$FE,$DD,$10,$D4,$80

qr194:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$08,$50,$6E,$82,$EA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$6F,$AE,$C1,$6C,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$51,$E5,$61,$FC,$4D,$EA,$D0,$61,$02,$24,$30,$2B,$66,$7D,$06,$4E
	.byte	$88,$59,$D4,$19,$06,$BE,$9A,$05,$72,$82,$7F,$EF,$48,$25,$6E,$A7
	.byte	$DF,$6C,$E9,$C3,$59,$79,$E1,$29,$29,$E6,$5C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$4A,$03,$1A,$BA,$9F,$CF,$B5,$D4,$D4,$80,$6E,$B2
	.byte	$7C,$F5,$05,$37,$1A,$AF,$E8,$75,$6E,$00

qr195:	.byte	33
	.byte	$FE,$41,$9D,$3F,$C1,$6C,$37,$10,$6E,$9C,$A9,$8B,$B7,$5A,$00,$75
	.byte	$DB,$A2,$F5,$F2,$EC,$15,$3A,$41,$07,$FA,$AA,$AA,$FE,$00,$19,$2D
	.byte	$00,$FB,$EB,$C2,$D5,$6C,$A0,$4E,$B0,$F4,$F6,$7B,$84,$06,$8E,$67
	.byte	$E6,$47,$B5,$15,$2C,$86,$E5,$75,$F9,$2E,$CD,$1E,$81,$55,$18,$D9
	.byte	$3A,$B4,$3B,$AB,$D3,$C9,$30,$E0,$5F,$F2,$DF,$96,$58,$A1,$5C,$2E
	.byte	$50,$8C,$CA,$7D,$05,$25,$94,$C9,$73,$EB,$1E,$5E,$9C,$B8,$1D,$52
	.byte	$D8,$2C,$FC,$BF,$4B,$C2,$FC,$80,$50,$7F,$45,$7F,$A6,$72,$6B,$50
	.byte	$46,$65,$B1,$DB,$AF,$1C,$0F,$8D,$D7,$75,$C0,$DE,$EB,$1D,$0C,$91
	.byte	$05,$59,$1C,$CC,$FE,$CA,$6B,$B9,$00

qr196:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$18,$50,$6E,$82,$EA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6D,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$50,$2D,$51,$FC,$4C,$8B,$50,$61,$C0,$20,$30,$23,$E6,$7D,$06,$70
	.byte	$0A,$59,$D5,$89,$86,$BE,$96,$A1,$72,$82,$D7,$AF,$48,$25,$EA,$97
	.byte	$DF,$6D,$99,$43,$59,$40,$A5,$29,$2B,$7E,$5C,$FB,$80,$5A,$5C,$7F
	.byte	$FB,$BC,$6B,$90,$4E,$03,$1A,$BA,$9F,$CF,$B5,$D6,$D4,$80,$6E,$A2
	.byte	$7C,$F5,$05,$77,$1A,$AF,$E8,$75,$6E,$00

qr197:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$08,$50,$6E,$92,$6A,$BB,$75,$9F,$C5,$DB,$A1
	.byte	$2F,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$02,$13,$00,$FB,$E6,$55
	.byte	$54,$8E,$81,$FC,$41,$B2,$D0,$60,$F2,$E4,$30,$2D,$7D,$FD,$06,$18
	.byte	$9D,$59,$D4,$2C,$36,$BE,$86,$27,$72,$82,$A3,$27,$48,$26,$47,$A7
	.byte	$DF,$6D,$98,$43,$59,$2A,$91,$29,$2B,$72,$7C,$FB,$80,$6A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$46,$43,$19,$BA,$DF,$CF,$B5,$D4,$E4,$80,$6E,$AA
	.byte	$FC,$F5,$05,$83,$1A,$AF,$E8,$F5,$6E,$00

qr198:	.byte	33
	.byte	$FE,$41,$FD,$3F,$C1,$6C,$13,$10,$6E,$9C,$A5,$8B,$B7,$5A,$18,$75
	.byte	$DB,$A2,$F1,$F2,$EC,$15,$3F,$01,$07,$FA,$AA,$AA,$FE,$00,$1A,$6D
	.byte	$00,$FB,$EB,$4A,$D5,$76,$E0,$DE,$B0,$CD,$D6,$03,$84,$0E,$8E,$55
	.byte	$E6,$4A,$7D,$00,$2C,$87,$69,$7A,$F9,$2D,$2B,$1D,$01,$54,$F3,$19
	.byte	$1A,$B4,$06,$6B,$C3,$C9,$12,$50,$5F,$F2,$F7,$96,$70,$A1,$44,$B2
	.byte	$63,$8C,$C6,$61,$15,$25,$94,$C9,$76,$EB,$1E,$CB,$9E,$B8,$1D,$13
	.byte	$19,$5C,$FC,$8F,$8B,$EA,$FC,$80,$70,$7F,$45,$7F,$B6,$50,$6B,$50
	.byte	$46,$54,$B1,$CB,$AD,$05,$0F,$8D,$D7,$70,$C0,$DE,$EA,$1C,$8C,$91
	.byte	$05,$18,$1C,$CC,$FE,$8B,$CB,$B9,$00

qr199:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$0F,$9D,$10,$6E,$95,$81,$0B,$B7,$46,$69,$B5
	.byte	$DB,$A3,$D5,$62,$EC,$12,$B1,$39,$07,$FA,$AA,$AA,$FE,$01,$94,$55
	.byte	$00,$DA,$4F,$D8,$A0,$92,$91,$19,$AC,$B5,$F2,$91,$CD,$2A,$90,$6D
	.byte	$05,$C7,$66,$61,$9A,$5D,$AB,$40,$1A,$A1,$CA,$8D,$C8,$70,$08,$DD
	.byte	$06,$C5,$43,$B9,$2A,$ED,$F8,$58,$1C,$7C,$E6,$E3,$A6,$7A,$2C,$A2
	.byte	$B4,$02,$F4,$3B,$4C,$01,$04,$E4,$64,$9A,$DA,$09,$D5,$9C,$8F,$00
	.byte	$7A,$92,$C4,$EF,$3D,$11,$F9,$00,$43,$C1,$45,$BF,$97,$5C,$EB,$10
	.byte	$42,$15,$71,$CB,$AA,$33,$9F,$DD,$D5,$F9,$F8,$3E,$E8,$44,$61,$27
	.byte	$05,$97,$64,$2F,$FE,$AF,$59,$F0,$00

qr200:	.byte	33
	.byte	$FE,$F7,$26,$3F,$C1,$48,$B1,$50,$6E,$91,$13,$4B,$B7,$43,$3C,$E5
	.byte	$DB,$A9,$9A,$42,$EC,$17,$75,$25,$07,$FA,$AA,$AA,$FE,$01,$88,$64
	.byte	$00,$E6,$DD,$91,$F9,$8C,$44,$4C,$F9,$D7,$BB,$AD,$5F,$6E,$A7,$71
	.byte	$74,$09,$36,$61,$9A,$5D,$CD,$31,$DD,$BC,$5A,$C4,$EC,$E2,$12,$08
	.byte	$53,$90,$8F,$5D,$B8,$A4,$D2,$C4,$6D,$BB,$D0,$8B,$A6,$7A,$37,$2F
	.byte	$73,$1E,$86,$24,$68,$93,$49,$8A,$31,$CF,$8F,$49,$47,$D5,$AA,$5A
	.byte	$8B,$55,$D8,$E7,$1D,$11,$F9,$00,$74,$DD,$44,$7F,$83,$CE,$AA,$30
	.byte	$53,$40,$31,$9B,$A6,$7A,$BF,$CD,$D3,$3E,$E4,$4E,$EA,$C4,$61,$27
	.byte	$05,$0B,$15,$E8,$FE,$DD,$10,$D4,$80

qr201:	.byte	29
	.byte	$FE,$74,$9B,$FC,$15,$78,$50,$6E,$9C,$EA,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$CF,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A2,$55
	.byte	$51,$2F,$B1,$FC,$54,$E9,$D0,$60,$0A,$94,$30,$2F,$B4,$7D,$06,$6A
	.byte	$08,$59,$D7,$B9,$B6,$BE,$8F,$8A,$32,$82,$AF,$7F,$48,$24,$8B,$E7
	.byte	$DF,$6E,$A9,$C3,$59,$18,$05,$29,$28,$7F,$FC,$FB,$80,$7A,$5C,$7F
	.byte	$FB,$0C,$6B,$90,$4C,$43,$18,$BA,$AF,$CF,$A5,$D4,$84,$80,$6E,$B1
	.byte	$7C,$F5,$05,$E3,$1A,$AF,$E8,$D5,$6E,$00

qr202:	.byte	29
	.byte	$FE,$72,$43,$FC,$10,$F0,$50,$6E,$9C,$EA,$BB,$75,$68,$D5,$DB,$AA
	.byte	$AF,$AE,$C1,$32,$61,$07,$FA,$AA,$AF,$E0,$09,$33,$00,$C7,$6E,$50
	.byte	$C5,$85,$1D,$8D,$A7,$96,$50,$61,$99,$66,$38,$07,$B6,$90,$B0,$A2
	.byte	$37,$DB,$DC,$9A,$A6,$BE,$84,$8A,$43,$45,$0E,$25,$48,$24,$C8,$AF
	.byte	$FF,$FB,$C4,$75,$83,$28,$D3,$21,$0B,$EE,$FC,$FB,$80,$48,$9C,$63
	.byte	$FB,$04,$6B,$90,$5E,$63,$1B,$BA,$51,$1F,$C5,$D3,$9C,$A0,$6E,$93
	.byte	$7C,$F5,$05,$24,$06,$DF,$ED,$D5,$6E,$00

qr203:	.byte	29
	.byte	$FE,$42,$9B,$FC,$16,$E8,$50,$6E,$9C,$6A,$BB,$75,$AF,$C5,$DB,$A2
	.byte	$CF,$AE,$C1,$50,$E1,$07,$FA,$AA,$AF,$E0,$01,$53,$00,$FB,$EE,$55
	.byte	$51,$0E,$01,$FC,$7A,$96,$50,$60,$99,$E4,$30,$26,$BD,$FD,$06,$70
	.byte	$F4,$59,$D4,$1F,$AE,$BE,$90,$3E,$32,$82,$5A,$E5,$48,$25,$C3,$A7
	.byte	$DF,$61,$D1,$C3,$59,$5A,$51,$29,$29,$BE,$FC,$FB,$80,$79,$5C,$7F
	.byte	$FB,$04,$6B,$90,$42,$43,$19,$BA,$A7,$CF,$AD,$D7,$94,$80,$EE,$B3
	.byte	$7C,$F5,$05,$63,$1A,$AF,$EB,$D5,$6E,$00

qr204:	.byte	29
	.byte	$FE,$0A,$9B,$FC,$15,$68,$50,$6E,$84,$6A,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$4F,$AE,$C1,$63,$E1,$07,$FA,$AA,$AF,$E0,$0D,$53,$00,$FB,$CE,$55
	.byte	$54,$81,$A1,$FC,$40,$88,$D0,$60,$E9,$24,$30,$21,$FE,$7D,$06,$5A
	.byte	$49,$59,$D5,$69,$26,$BE,$81,$86,$32,$82,$07,$9F,$48,$25,$0E,$C7
	.byte	$DF,$68,$9A,$C3,$59,$0B,$75,$29,$2B,$38,$7C,$FB,$80,$79,$5C,$7F
	.byte	$FB,$94,$6B,$90,$49,$03,$1B,$BA,$A7,$CF,$B5,$D4,$94,$80,$6E,$B3
	.byte	$7C,$F5,$05,$E7,$1A,$AF,$ED,$D5,$6E,$00

qr205:	.byte	29
	.byte	$FE,$2A,$9B,$FC,$15,$28,$50,$6E,$82,$6A,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$53,$00,$FB,$D6,$55
	.byte	$52,$C1,$61,$FC,$6D,$E9,$D0,$61,$1B,$10,$30,$2A,$E6,$DD,$06,$3C
	.byte	$A2,$59,$D7,$A9,$7E,$BE,$8D,$3E,$32,$82,$92,$5F,$48,$27,$41,$57
	.byte	$DF,$66,$92,$43,$59,$19,$65,$29,$29,$B0,$7C,$FB,$80,$69,$5C,$7F
	.byte	$FB,$94,$6B,$90,$49,$03,$1B,$BA,$ED,$CF,$AD,$D5,$A4,$80,$EE,$A1
	.byte	$FC,$F5,$05,$E3,$1A,$AF,$EA,$75,$6E,$00

qr206:	.byte	29
	.byte	$FE,$21,$13,$FC,$12,$3B,$D0,$6E,$A4,$08,$BB,$74,$87,$25,$DB,$A4
	.byte	$B7,$2E,$C1,$18,$D9,$07,$FA,$AA,$AF,$E0,$11,$9D,$00,$EF,$85,$DE
	.byte	$24,$06,$62,$72,$69,$88,$33,$EE,$78,$EC,$D3,$A8,$EA,$C5,$E5,$DA
	.byte	$D7,$61,$37,$68,$10,$86,$69,$91,$FC,$BA,$27,$64,$C6,$1B,$A4,$64
	.byte	$51,$6E,$FA,$A0,$D6,$E1,$9D,$CA,$A8,$A3,$C4,$F8,$00,$61,$64,$5F
	.byte	$FA,$AA,$6B,$70,$50,$CD,$10,$BA,$C6,$4F,$8D,$D3,$47,$0E,$6E,$B1
	.byte	$1F,$7B,$05,$AB,$F9,$2F,$EA,$4D,$8D,$80

qr207:	.byte	29
	.byte	$FE,$72,$9B,$FC,$15,$58,$50,$6E,$9C,$EA,$BB,$75,$7F,$C5,$DB,$A5
	.byte	$CF,$AE,$C1,$65,$E1,$07,$FA,$AA,$AF,$E0,$0F,$53,$00,$FB,$A6,$55
	.byte	$56,$EF,$81,$FC,$56,$C9,$50,$60,$B9,$90,$30,$21,$3C,$7D,$06,$46
	.byte	$AA,$59,$D6,$BD,$AE,$BE,$9A,$0A,$32,$82,$2B,$BF,$48,$25,$01,$C7
	.byte	$DF,$66,$D0,$C3,$59,$19,$75,$29,$28,$3C,$3C,$FB,$80,$7B,$5C,$7F
	.byte	$FB,$1C,$6B,$90,$4E,$43,$18,$BA,$EF,$CF,$A5,$D7,$84,$80,$6E,$A1
	.byte	$7C,$F5,$05,$C3,$1A,$AF,$EC,$75,$6E,$00

qr208:	.byte	33
	.byte	$FE,$2F,$3D,$3F,$C1,$53,$C3,$10,$6E,$83,$43,$8B,$B7,$5D,$69,$75
	.byte	$DB,$A4,$19,$F2,$EC,$16,$C3,$41,$07,$FA,$AA,$AA,$FE,$00,$E5,$7D
	.byte	$00,$FB,$D0,$DA,$D5,$6A,$97,$86,$B0,$C9,$89,$F9,$84,$15,$21,$A5
	.byte	$E6,$4A,$26,$B5,$2C,$87,$6A,$0F,$F9,$2C,$AC,$61,$21,$55,$21,$25
	.byte	$1A,$B4,$FE,$D0,$43,$C9,$0E,$A7,$8F,$F2,$E3,$99,$D8,$A1,$57,$3D
	.byte	$97,$8C,$CD,$66,$BF,$25,$95,$62,$07,$EB,$1E,$2C,$67,$18,$1D,$5A
	.byte	$66,$7C,$FC,$92,$51,$52,$FC,$80,$67,$4F,$45,$7F,$B9,$EA,$6B,$50
	.byte	$45,$91,$B1,$FB,$AC,$BC,$8F,$95,$D4,$04,$C0,$DE,$EA,$65,$8C,$91
	.byte	$05,$E5,$5C,$CC,$FE,$90,$6B,$B9,$00

qr209:	.byte	29
	.byte	$FE,$B1,$83,$FC,$17,$70,$90,$6E,$B2,$D2,$BB,$75,$0E,$05,$DB,$A1
	.byte	$DE,$2E,$C1,$47,$6D,$07,$FA,$AA,$AF,$E0,$08,$0F,$00,$CE,$61,$49
	.byte	$79,$0D,$B9,$1F,$EC,$A6,$E8,$83,$21,$E5,$F7,$3E,$E8,$4C,$C1,$2E
	.byte	$E8,$D7,$EE,$BB,$5D,$30,$BF,$39,$6E,$F3,$96,$D8,$54,$55,$69,$FF
	.byte	$3C,$C2,$EE,$7B,$BA,$0B,$84,$EE,$3D,$61,$8D,$FC,$80,$6A,$D4,$47
	.byte	$F8,$EF,$EB,$B0,$50,$1F,$19,$BA,$E8,$DF,$D5,$D0,$AC,$63,$EE,$8E
	.byte	$C4,$17,$05,$A2,$DD,$BF,$EC,$84,$A9,$00

qr210:	.byte	29
	.byte	$FE,$54,$9B,$FC,$16,$08,$50,$6E,$9C,$AA,$BB,$75,$4F,$C5,$DB,$A7
	.byte	$4F,$AE,$C1,$57,$E1,$07,$FA,$AA,$AF,$E0,$0A,$13,$00,$FB,$DE,$55
	.byte	$56,$08,$C1,$FC,$4D,$CA,$D0,$61,$A8,$30,$30,$24,$EE,$7D,$06,$34
	.byte	$2A,$59,$D5,$AC,$86,$BE,$9D,$29,$72,$82,$7E,$C7,$48,$26,$E4,$87
	.byte	$DF,$69,$93,$43,$59,$33,$E5,$29,$2B,$3B,$FC,$FB,$80,$5C,$5C,$7F
	.byte	$FB,$A4,$6B,$90,$43,$43,$1A,$BA,$A7,$CF,$A5,$D4,$94,$80,$6E,$A8
	.byte	$FC,$F5,$05,$63,$1A,$AF,$E9,$F5,$6E,$00

qr211:	.byte	29
	.byte	$FE,$2C,$9B,$FC,$15,$38,$50,$6E,$82,$AA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6C,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$50,$E5,$51,$FC,$68,$8A,$50,$61,$51,$20,$30,$27,$66,$7D,$06,$58
	.byte	$28,$59,$D5,$2F,$0E,$BE,$93,$19,$72,$82,$8A,$EF,$48,$26,$6F,$97
	.byte	$DF,$62,$C1,$43,$59,$3B,$61,$29,$29,$22,$5C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$4E,$03,$1A,$BA,$FF,$CF,$B5,$D4,$D4,$80,$6E,$AA
	.byte	$7C,$F5,$05,$F7,$1A,$AF,$EE,$75,$6E,$00

qr212:	.byte	29
	.byte	$FE,$08,$9B,$FC,$15,$4C,$50,$6E,$84,$AA,$BB,$75,$CF,$C5,$DB,$A5
	.byte	$2F,$AE,$C1,$62,$E1,$07,$FA,$AA,$AF,$E0,$0D,$13,$00,$FB,$CA,$55
	.byte	$57,$C1,$B1,$FC,$54,$C9,$D0,$61,$C0,$24,$30,$27,$36,$7D,$06,$0E
	.byte	$0A,$59,$D5,$5B,$26,$BE,$8D,$BE,$32,$82,$2E,$5F,$48,$24,$4B,$C7
	.byte	$DF,$6A,$BA,$C3,$59,$39,$F5,$29,$2B,$F6,$7C,$FB,$80,$68,$5C,$7F
	.byte	$FB,$84,$6B,$90,$45,$43,$1B,$BA,$E7,$CF,$B5,$D6,$84,$80,$6E,$B2
	.byte	$FC,$F5,$05,$A3,$1A,$AF,$ED,$D5,$6E,$00

qr213:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$0C,$50,$6E,$82,$AA,$BB,$75,$DF,$C5,$DB,$A4
	.byte	$2F,$AE,$C1,$6E,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$56,$85,$41,$FC,$6D,$EB,$D0,$61,$4A,$24,$30,$27,$2E,$7D,$06,$7A
	.byte	$2A,$59,$D4,$BB,$8E,$BE,$83,$99,$32,$82,$26,$6F,$48,$27,$60,$87
	.byte	$DF,$6C,$E0,$C3,$59,$43,$25,$29,$28,$E4,$5C,$FB,$80,$4A,$5C,$7F
	.byte	$FA,$3C,$6B,$90,$42,$03,$1A,$BA,$BF,$CF,$B5,$D4,$D4,$80,$6E,$BA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$E8,$75,$6E,$00

qr214:	.byte	29
	.byte	$FE,$F6,$43,$FC,$14,$85,$50,$6E,$91,$1C,$BB,$74,$2D,$85,$DB,$A9
	.byte	$C2,$2E,$C1,$72,$71,$07,$FA,$AA,$AF,$E0,$1B,$37,$00,$E6,$D0,$8F
	.byte	$9F,$E4,$E8,$D8,$F4,$BD,$66,$BA,$4B,$42,$79,$08,$B7,$90,$B0,$8C
	.byte	$BE,$CB,$9F,$CC,$C5,$D3,$36,$A7,$56,$10,$72,$B1,$93,$48,$AA,$DE
	.byte	$FB,$FA,$CE,$F5,$82,$53,$B3,$60,$0F,$3A,$91,$FD,$00,$68,$CC,$77
	.byte	$F9,$77,$2A,$30,$5E,$27,$1A,$BA,$53,$1F,$DD,$D0,$ED,$A4,$EE,$BC
	.byte	$4A,$2F,$05,$41,$53,$8F,$EF,$18,$D8,$80

qr215:	.byte	29
	.byte	$FE,$23,$13,$FC,$12,$3B,$D0,$6E,$A4,$28,$BB,$74,$85,$25,$DB,$A4
	.byte	$97,$2E,$C1,$19,$D9,$07,$FA,$AA,$AF,$E0,$11,$9D,$00,$EF,$85,$DE
	.byte	$23,$4E,$42,$72,$44,$E8,$B3,$EE,$52,$EC,$D3,$A4,$2A,$C5,$E5,$F4
	.byte	$F4,$61,$36,$CD,$10,$86,$75,$A9,$FC,$BA,$FF,$24,$C6,$18,$E9,$74
	.byte	$51,$61,$AA,$20,$D6,$F2,$9D,$CA,$AA,$21,$C4,$F8,$00,$53,$64,$5F
	.byte	$FB,$BA,$6B,$70,$50,$CD,$10,$BA,$E6,$4F,$8D,$D0,$47,$0E,$6E,$A1
	.byte	$1F,$7B,$05,$6B,$F9,$2F,$E8,$4D,$8D,$80

qr216:	.byte	29
	.byte	$FE,$40,$9B,$FC,$16,$C8,$50,$6E,$9C,$8A,$BB,$75,$AD,$C5,$DB,$A2
	.byte	$AF,$AE,$C1,$52,$E1,$07,$FA,$AA,$AF,$E0,$01,$13,$00,$FB,$EE,$55
	.byte	$55,$2A,$01,$FC,$79,$97,$D0,$61,$53,$E4,$30,$23,$E5,$FD,$06,$50
	.byte	$15,$59,$D4,$3E,$36,$BE,$9B,$1E,$72,$82,$82,$A5,$48,$26,$A9,$A7
	.byte	$DF,$68,$89,$C3,$59,$22,$91,$29,$29,$3A,$FC,$FB,$80,$79,$5C,$7F
	.byte	$FA,$84,$6B,$90,$4E,$43,$19,$BA,$C7,$CF,$AD,$D5,$94,$80,$EE,$B3
	.byte	$7C,$F5,$05,$A3,$1A,$AF,$EF,$D5,$6E,$00

qr217:	.byte	29
	.byte	$FE,$28,$9B,$FC,$15,$18,$50,$6E,$82,$8A,$BB,$75,$DD,$C5,$DB,$A4
	.byte	$4F,$AE,$C1,$6F,$E1,$07,$FA,$AA,$AF,$E0,$0E,$13,$00,$FB,$DE,$55
	.byte	$57,$29,$61,$FC,$7D,$EA,$50,$61,$E0,$24,$30,$20,$26,$7D,$06,$68
	.byte	$49,$59,$D7,$9C,$8E,$BE,$89,$01,$32,$82,$E6,$EF,$48,$26,$AD,$A7
	.byte	$DF,$65,$E8,$43,$59,$31,$21,$29,$28,$FA,$5C,$FB,$80,$5A,$5C,$7F
	.byte	$FA,$BC,$6B,$90,$46,$03,$1A,$BA,$DF,$CF,$B5,$D6,$D4,$80,$6E,$AA
	.byte	$7C,$F5,$05,$B7,$1A,$AF,$EA,$75,$6E,$00

qrptrl:	.word	qr001,qr002,qr003,qr004,qr005,qr006,qr007,qr008
	.word	qr009,qr010,qr011,qr012,qr013,qr014,qr015,qr016
	.word	qr017,qr018,qr019,qr020,qr021,qr022,qr023,qr024
	.word	qr025,qr026,qr027,qr028,qr029,qr030,qr031,qr032
	.word	qr033,qr034,qr035,qr036,qr037,qr038,qr039,qr040
	.word	qr041,qr042,qr043,qr044,qr045,qr046,qr047,qr048
	.word	qr049,qr050,qr051,qr052,qr053,qr054,qr055,qr056
	.word	qr057,qr058,qr059,qr060,qr061,qr062,qr063,qr064
	.word	qr065,qr066,qr067,qr068,qr069,qr070,qr071,qr072
	.word	qr073,qr074,qr075,qr076,qr077,qr078,qr079,qr080
	.word	qr081,qr082,qr083,qr084,qr085,qr086,qr087,qr088
	.word	qr089,qr090,qr091,qr092,qr093,qr094,qr095,qr096
	.word	qr097,qr098,qr099,qr100,qr101,qr102,qr103,qr104
	.word	qr105,qr106,qr107,qr108,qr109,qr110,qr111,qr112
	.word	qr113,qr114,qr115,qr116,qr117,qr118,qr119,qr120
	.word	qr121,qr122,qr123,qr124,qr125,qr126,qr127,qr128

qrptrh:	.word	qr129,qr130,qr131,qr132,qr133,qr134,qr135,qr136
	.word	qr137,qr138,qr139,qr140,qr141,qr142,qr143,qr144
	.word	qr145,qr146,qr147,qr148,qr149,qr150,qr151,qr152
	.word	qr153,qr154,qr155,qr156,qr157,qr158,qr159,qr160
	.word	qr161,qr162,qr163,qr164,qr165,qr166,qr167,qr168
	.word	qr169,qr170,qr171,qr172,qr173,qr174,qr175,qr176
	.word	qr177,qr178,qr179,qr180,qr181,qr182,qr183,qr184
	.word	qr185,qr186,qr187,qr188,qr189,qr190,qr191,qr192
	.word	qr193,qr194,qr195,qr196,qr197,qr198,qr199,qr200
	.word	qr201,qr202,qr203,qr204,qr205,qr206,qr207,qr208
	.word	qr209,qr210,qr211,qr212,qr213,qr214,qr215,qr216
	.word	qr217

tapecode:
.scope tape
cout	=	$FDED		; character out sub
crout	=	$FD8E		; CR out sub
tapein	=	$C060		; read tape interface
warm	=	$FF69		; back to monitor
clear	=	$FC58		; clear screen
inflate	=	$BA00		; inflate code start

; zero page parameters

begload	=	$00		; begin load location LSB/MSB
endload	=	$02		; end load location LSB/MSB
chksum	=	$04		; checksum location
remain	=	$04		; checksum remainder location
pointer	=	$0C		; LSB/MSB pointer
inf_zp	=	$00		; inflate code zero page vars (10)

	.org	$BE00

	;jsr	resetscreen	; back to 40 col mode
	;jsr	warm

getparams:			; client params
	lda	#<ld_beg	; store begin location LSB
	sta	begload
	lda	#>ld_beg	; store begin location MSB
	sta	begload+1
				; end of params + 1 for checksum
	lda	#<end		; store end location LSB
	sta	endload
	lda	#>end		; store end location MSB
	sta	endload+1

retry:
	jsr	readtape	; get the code

;; new approach, check for error after rts
	bne	retry		; if readtape gets invalid length try again
				; this is required in the event the smartphone
				; QR scan app "dings" on detect

	jsr	resetscreen	; back to 40 col mode

	lda	#<param		; print param message
	ldy	#>param
	jsr	print
	jsr	sumcheck	; check the data
	lda	#<okm
	ldy	#>okm
	jsr	print
	jsr	crout		; 2 linefeeds
	jsr	crout
	lda	#<loadm		; print loading message
	ldy	#>loadm
	jsr	print

;; get the game

	lda	ld_beg		; store begin location LSB
	sta	begload
	lda	ld_beg+1	; store begin location MSB
	sta	begload+1
				; end of params + 1 for checksum
	lda	ld_end		; store end location LSB
	sta	endload
	lda	ld_end+1	; store end location MSB
	sta	endload+1

	jsr	readtape	; get the code
	beq	dosum		; if readtape ok to load code branch to dosum
	jmp	error		; else jmp to error
dosum:
	jsr	sumcheck	; check the data

	lda	inf_flag	; if inf_flag = 0 runit
	bne	inf
	jmp	runit
inf:
	jsr	crout
	lda	#<infm
	ldy	#>infm
	jsr	print

	lda	inf_src		;src lsb
	sta	inf_zp+0
	lda	inf_src+1	;src msb
	sta	inf_zp+1
	lda	inf_dst		;dst lsb
	sta	inf_zp+2
	lda	inf_dst+1	;dst msb
	sta	inf_zp+3

	jmp	lastpage	;jump to page BF since inflate will kill BE

resetscreen:
	sta	clran3		; disable double lowres
	sta	mixedoff	; disable mixed
	sta	texton		; text mode on
	sta	a80off		; 40 col
	sta	a80soff		; page between main and main
	sta	page1on		; page one one
	jsr	clear		; clear screen
	lda	#40		; set wndleft to 40 col
	sta	wndleft
	rts

readtape:
	lda	begload		; load begin LSB location
	sta	store+1		; store it
	lda	begload+1	; load begin MSB location
	sta	store+2		; store it

	ldx	#0		; set X to 0
	lda	#1		; set A to 0 

nsync:	bit	tapein		; 4 cycles, sync bit ; first pulse
	bpl	nsync		; 2 + 1 cycles

main:	ldy	#0		; 2 set Y to 0 

psync:	bit	tapein		; 
	bmi	psync

ploop:	iny			; 2 cycles
	bit	tapein		; 4 cycles
	bpl	ploop		; 2 +1 if branch, +1 if in another page
				; total ~9 cycles

	cpy	#$40		; 2 cycles if Y - $40 > 0 endcode (770Hz)
	bpl	endcode		; 2(3)

	cpy	#$15		; 2 cycles if Y - $15 > 0 main (2000Hz)
	bpl	main		; 2(3)

	cpy	#$07		; 2, if Y<, then clear carry, if Y>= set carry
store:	rol	store+1,x	; 7, roll carry bit into store
	ldy	#0		; 2
	asl			; 2 A*=2
	bne	main		; 2(3)
	lda	#1		; 2
	inx			; 2 cycles
	bne	main		; 2(3)
	inc	store+2		; 6 cycles
	jmp	main		; 3 cycles
				; 34 subtotal max
				; 36 subtotal max
endcode:  
	txa			; write end of file location + 1
	clc
	adc	store+1
	sta	store+1
	bcc	endcheck	; LSB didn't roll over to zero
	inc	store+2		; did roll over to zero, inc MSB
endcheck:			; check for match of expected length
	lda	endload
	cmp	store+1

	;bne	error
	bne	enderror	; return and check for bne from caller

	lda	endload+1
	cmp	store+2
	;bne	error		; fix need to reset screen, etc...
enderror:			; return and check for bne from caller
	rts			; return to caller

sumcheck:
	sec			; set carry for sub operation
	lda	endload		; load end low
	sbc	begload		; sub beg load low with carry
	sta	remain		; save remainder
	;tay			; y will be our number of byte starting
				; with remainder of bytes to chksum
	lda	endload+1	; load end high
	sbc	begload+1	; sub beg load high with carry
	tax			; x will be our number of 256 byte blocks
	inx			; + 1

	lda	begload		; setup pointer low
	sta	pointer
	lda	begload+1	; setup pointer high
	sta	pointer+1
	dec	pointer+1

	lda	#$ff		; init checksum
sumblockloop:
	ldy	#0
	inc	pointer+1
	dex
	bne	sumloop
	ldy	remain		; load remainder
	beq	endsum		; 0?
remainder:
	dey			; array base 0
	eor	(pointer),y
	cpy	#0
	bne	remainder
	beq	endsum
sumloop:
	eor	(pointer),y
	iny
	bne	sumloop
	beq	sumblockloop
endsum:


;sumcheck:
;	lda	#0
;	sta	pointer
;	lda	begload+1
;	sta	pointer+1
;	lda	#$ff		; init checksum
;	ldy	begload
;sumloop:
;	eor	(pointer),y
;	;last page?
;	ldx	pointer+1
;	cpx	endload+1
;	beq	last
;	iny
;	bne	sumloop
;	inc	pointer+1
;	bne	sumloop
;last:
;	iny
;	cpy	endload
;	bcc	sumloop

	sta	chksum		; for debugging
	lda	chksum

	bne	sumerror	; chksum = 0?
	rts			; return to caller
sumerror:
	lda	#<chkm
	ldy	#>chkm
	jsr	print
error:
	bit	mixed		; still in mixed mode?
	bpl	errorcont	; no then carry on
	jsr	resetscreen	; else reset screen first
errorcont:
	lda	#<errm
	ldy	#>errm
	jsr	print
	jmp	warm	
ok:
	lda	#<okm
	ldy	#>okm
print:
	sta	pointer
	sty	pointer+1
	ldy	#0
	lda	(pointer),y	; load initial char
print1:	ora	#$80
	jsr	cout
	iny
	lda	(pointer),y
	bne	print1
	rts

lastpage:
	jsr	inflate		; decompress

	lda	inf_end		;dst end +1 lsb
	cmp	inf_zp+2
	bne	error
	lda	inf_end+1	;dst end +1 msb
	cmp	inf_zp+3
	bne	error
runit:
	lda	warm_flag	; if warm_flag = 1 warm boot
	bne	warmit
	jmp	(runcode)	; run it
warmit:
	jmp	warm		; warm it

param:	.asciiz	"LOADING PARAMETERS "
chkm:	.asciiz	"CHKSUM "
okm:	.asciiz	"OK"
errm:	.asciiz	"ERROR"
infm:	.byte	$0D,"INFLATING ",$00
ld_beg:
	.org	*+2
ld_end:
	.org	*+2
inf_src:
	.org	*+2
runcode:
inf_dst:
	.org	*+2
inf_end:
	.org	*+2
inf_flag:
	.org	*+1
warm_flag:
	.org	*+1
loadm:
	.org	*+60
chk:
	.org	*+1
end:
.endscope

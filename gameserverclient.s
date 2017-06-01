;;apple game server client

;; (c) 2012, 2017 Egan Ford (egan@sense.net)

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

.include	"titles.inc"

;; game qrcodes

.include	"qrcodes.inc"

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

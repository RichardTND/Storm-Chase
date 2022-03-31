;---------------------------------------
;Storm Chase (PAL/NTSC)
;(C)2020 The New Dimension
;Programmed on theC64 full size
;
;64TASS Version
;---------------------------------------

scrolltext = $0900
titlemap = $3000
titlecol = $3400
gamemap  = $3800
gamecol  = $3c00
endmap   = $4000
endcol   = $4400

floodchar1 = $07c1
floodchar2 = $07c1-40
floodchar3 = $07c1-80
floodchar4 = $07c1-120
floodchar5 = $07c1-160
floodchar6 = $07c1-200
floodchar7 = $07c1-240
floodchar8 = $07c1-280
floodcol1 = $dbc1
floodcol2 = $dbc1-40
floodcol3 = $dbc1-80
floodcol4 = $dbc1-120
floodcol5 = $dbc1-160
floodcol6 = $dbc1-200
floodcol7 = $dbc1-240
floodcol8 = $dbc0-280

musicinit = $1000
musicplay = $1003

titlemus = $00
getreadyjingle = $01
gameoverjingle = $02
deadjingle = $03
rainsfx  = $04
stormsfx = $05
collectsfx = $06
floodsfx = $07

;---------------------------------------
;Import Scroll text 
		*=$0900-2
		.binary "bin/scrolltext.prg"
;---------------------------------------
;Import music and sound effects (DMC V2.0)
		*=$1000-2
		.binary "bin/musicsfx.prg"
;----------------------------------------
;Import sprite data 
		*=$2000-2
		.binary "bin/sprites.prg"
;-----------------------------------------
;Import charset data 
		*=$2800-2
		.binary "bin/charset.prg"
;-----------------------------------------
;Import title screen 
		*=$3000-2
		.binary "bin/titlescreen.prg"
;-----------------------------------------
;Import game screen 
		*=$3800-2
		.binary "bin/gamescreen.prg"
;-----------------------------------------
;Import game end screen 
		*=$4000-2 
		.binary "bin/endscreen.prg"
;------------------------------------------		
		 
         *= $4800

         ;PAL/NTSC detect

         lda $02a6
         sta system

         ;Disable RUN/RESTORE

         lda #252
         sta 808

;---------------------------------------
;Front End
;---------------------------------------

titlescreen

         jsr killirqs

         lda #$00
         sta firebutton

         lda #$00
         sta $d020
         sta $d021
         ldx #$00
copyfescr
         lda titlemap,x
         sta $0400,x
         lda titlemap+$0100,x
         sta $0400+$0100,x
         lda titlemap+$0200,x
         sta $0400+$0200,x
         lda titlemap+$02e8,x
         sta $0400+$02e8,x
         lda titlecol,x
         sta $d800,x
         lda titlecol+$0100,x
         sta $d900,x
         lda titlecol+$0200,x
         sta $da00,x
         lda titlecol+$02e8,x
         sta $dae8,x
         inx
         bne copyfescr
         jsr maskpanel
         lda #$1a
         sta $d018
         lda #$08
         sta $d016
         lda #$09
         sta $d022
         lda #$01
         sta $d023
         ldx #<irq1
         ldy #>irq1
         lda #$7f
         stx $0314
         sty $0315
         lda #$7f
         sta $dc0d
         sta $dd0d
         lda #$36
         sta $d012
         lda #$1b
         sta $d011
         lda #$01
         sta $d01a
         sta $d019
         lda #$00
         jsr musicinit
         lda #<scrolltext
         sta messread+1
         lda #>scrolltext
         sta messread+2
         cli

;Main loop for title screen, until
;FIRE has been pressed.

titleloop
         lda #0
         sta synctimer
         cmp synctimer
         beq *-3

         jsr scroll
         jsr colscroll
         jsr colscroll

         lda $dc00
         lsr a
         lsr a
         lsr a
         lsr a
         lsr a
         bit firebutton
         ror firebutton
         bmi titleloop
         bvc titleloop
         lda #0
         sta firebutton

         jmp gamecode

;Message scrolling subroutine

scroll   lda xpos
         sec
         sbc #2
         and #7
         sta xpos
         bcs endscroll
         ldx #$00
shunt    lda $07c1,x
         sta $07c0,x
         lda #1
         sta $dbc0,x
         inx
         cpx #$28
         bne shunt

messread lda scrolltext
         cmp #$00
         bne storchr
         lda #<scrolltext
         sta messread+1
         lda #>scrolltext
         sta messread+2
         jmp messread
storchr  sta $07e7
         inc messread+1
         lda messread+1
         bne endscroll
         inc messread+2
endscroll rts

;Raster colour scroll subroutine

colscroll
         lda rasend
         sta $04
         ldx #rasend-rasstart
shift    lda rascol-1,x
         sta rascol,x
         dex
         bpl shift
         lda $04
         sta rasstart
         rts

;Interrupts for title screen

         ;Smooth scroll.

irq1     inc $d019
         lda $dc0d
         sta $dd0d
         lda #$2a
         sta $d012
         lda xpos
         sta $d016
         ldx #<irq2
         ldy #>irq2
         stx $0314
         sty $0315
         jmp $ea7e

         ;Normal text

irq2     inc $d019
         lda #$3c
         sta $d012
         lda #$08
         sta $d016
         ldx #<irq3
         ldy #>irq3
         stx $0314
         sty $0315
         jmp $ea7e

irq3     inc $d019
         ldy $d012

         ;Raster colour scroll

         ldx #rasend-rasstart
rasloop  lda rascol,x
         cpy $d012
         beq *-3
         sta $d021
         iny
         dex
         bpl rasloop
         ldx #$08
         dex
         bne *-1
         lda #0
         sta $d021
         lda #1
         sta synctimer

         lda #$f0
         sta $d012
         jsr pnplayer
         ldx #<irq1
         ldy #>irq1
         stx $0314
         sty $0315
         jmp $ea7e

;PAL/NTSC music player

pnplayer lda system
         cmp #1
         beq pal
         inc ntsctimer
         lda ntsctimer
         cmp #6
         beq timedright
pal      jsr musicplay
         rts
timedright lda #$00
         sta ntsctimer
         rts

;Title screen pointers

xpos     .byte 0
synctimer .byte 0
system   .byte 0
ntsctimer .byte 0
firebutton .byte $00

;Raster colour table

rascol
rasstart .byte $06,$06,$06,$06
         .byte $06,$06,$06,$06
         .byte $04,$06,$04,$04
         .byte $04,$04,$04,$04
         .byte $04,$04,$04,$04
         .byte $04,$04,$04,$04
         .byte $0e,$04,$0e,$0e
         .byte $0e,$0e,$0e,$0e
         .byte $0e,$0e,$0e,$0e
         .byte $03,$0e,$03,$03
         .byte $03,$03,$03,$03
         .byte $03,$03,$03,$03
         .byte $01,$03,$01,$01
         .byte $01,$01,$01,$01
         .byte $01,$07,$01,$07
         .byte $07,$07,$07,$07
         .byte $07,$07,$07,$07
         .byte $07,$0a,$07,$0a
         .byte $0a,$0a,$0a,$0a
         .byte $0a,$0a,$0a,$0a
         .byte $0a,$08,$0a,$08
         .byte $08,$08,$08,$08
         .byte $08,$08,$08,$08
         .byte $08,$08,$08,$08
         .byte $08,$08,$06,$08

rasend   .byte 6

;---------------------------------------
;Main game code
;---------------------------------------

gamecode jsr killirqs

         ;Initialise level counter

         ldx #$00
         stx levelcount

         ;Init fire press

         lda #$00
         sta firebutton

         ;Default level digits

         lda #$30
         sta level
         lda #$31
         sta level+1

         ;Setup game screen properties

         lda #$18  ;Allow multicolour
         sta $d016 ;charset
         lda #$1a  ;Font
         sta $d018
         lda #$09  ;Char mcol #1
         sta $d022
         lda #$01  ;Char mcol2
         sta $d023
         lda #$01  ;Spr mcol1
         sta $d025
         lda #$0b  ;Spr mcol2
         sta $d026

;Draw game screen

         ldx #$00
mkscrn   lda gamemap,x
         sta $0400,x
         lda gamemap+$0100,x
         sta $0500,x
         lda gamemap+$0200,x
         sta $0600,x
         lda gamemap+$02e8,x
         sta $06e8,x
         lda gamecol,x
         sta $d800,x
         lda gamecol+$0100,x
         sta $d900,x
         lda gamecol+$0200,x
         sta $da00,x
         lda gamecol+$02e8,x
         sta $dae8,x
         lda #$0d
         sta $dbe7
         inx
         bne mkscrn

;There are instances where the rain
;turns out to be white chars, when the
;screen was created. Repair this to
;make all raindrop chars blue

         ldx #$00
repaintsetup lda $0400,x
         cmp #$65
         bne notrain1
         lda #$0e
         sta $d800,x
notrain1 lda $0500,x
         cmp #$65
         bne notrain2
         lda #$0e
         sta $d900,x
notrain2 lda $0600,x
         cmp #$65
         bne notrain3
         lda #$0e
         sta $da00,x
notrain3 lda $06e8,x
         cmp #$65
         bne notrain4
         lda #$0e
         sta $dae8,x
notrain4 inx
         bne repaintsetup

;Setup game interrupts

         ldx #<gameirq
         ldy #>gameirq
         stx $0314
         sty $0315

         lda #$7f
         sta $dc0d
         sta $dd0d
         lda #$2e
         sta $d012
         lda #$1b
         sta $d011
         lda #$01
         sta $d01a
         sta $d019

         cli

;A brand new game has started, init
;player amount of lives, and score.

         ldx #$00
initscore lda #$30
         sta score,x
         inx
         cpx #6
         bne initscore

         lda #$33
         sta lives


gamereset

         lda #$00
         sta $d015

         ldx #$00
defaults
         lda startpos,x
         sta objpos,x
         inx
         cpx #$10
         bne defaults
         ldx #$00
resetcols
         lda defaultframe,x
         sta $07f8,x
         lda defaultcols,x
         sta $d027,x
         inx
         cpx #$08
         bne resetcols
         jsr widenarea

         lda #$00
         sta cloudtimer
         sta dropcounter
         sta animdelay
         sta animpointer
         sta objectcounter
         sta flowcount
         lda #$0a
         sta thunderpointer
         lda #$30
         sta left+1

;Setup level pointers. Read pointer
;level count to read reaquired table
;properties the store to necessary
;game property pointers or self-mod
;code.

         ldx levelcount
         lda dropspeedtable,x
         sta dropspeed
         lda dropamountable,x
         sta left
         lda levelinterval,x
         sta cloudduration
         lda leveltblo,x
         sta levelstore+1
         lda leveltbhi,x
         sta levelstore+2
         inx
         cpx #levelend-levelstart
         beq gamecomplete
         jmp continue
gamecomplete
         jmp endsequence

continue ldx #$00
levelstore
         lda level1,x
         sta behaviour,x
         inx
         cpx #50
         bne levelstore


;GET READY Prompt - Only for lives lost
;level complete, or new game start

getreadyprompt



         lda #getreadyjingle
         jsr musicinit

;Display GET READY text

         ldx #$00
displaygrtext
         lda txtgetready,x
         sta $059f,x
         lda #$01
         sta $d99f,x
         inx
         cpx #textend-textstart ;size
         bne displaygrtext

;Wait for fire to be pressed before
;starting a level

waitloop lda #$00
         sta synctimer
         cmp synctimer
         beq *-3
         jsr animatebg
         jsr maskpanel
         jsr checkflowcount
         lda $dc00
         lsr a
         lsr a
         lsr a
         lsr a
         lsr a
         bit firebutton
         ror firebutton
         bmi waitloop
         bvc waitloop

         lda #$00
         sta firebutton

;Restore background properties

         ldx #$00
bgrestore lda gamemap+$01a7,x
         sta $059f,x
         lda #$0e
         sta $d99f,x
         inx
         cpx #textend-textstart
         bne bgrestore

         lda #$00
         sta firebutton

;Automatically set start position
;and initialise frames, and level
;properties

         ldx #$00
resetpos lda startpos,x
         sta objpos,x
         inx
         cpx #$08
         bne resetpos

         lda #$ff
         sta $d015 ;Enable all sprites
         sta $d01c
         lda #%00000010
         sta $d01d
         sta $d017

;Main game loop

gameloop lda #0
         sta synctimer
         cmp synctimer
         beq *-3
         jsr widenarea
         jsr animate
         jsr animatebg
         jsr playercontrol
         jsr movecloud
         jsr cloudspitting
         jsr checkflowcount
         jsr gamecollision
         jsr maskpanel

         ;Thunder effect on cloud

         ldx thunderpointer
         lda thundercolour,x
         sta $d028
         inx
         cpx #thunderend-thunderstart
         beq forcegrey
         inc thunderpointer
         jmp gameloop

forcegrey lda #$0a
         sta thunderpointer
         jmp gameloop

;Expand sprite position by widening
;the max position for the sprite

widenarea ldx #$00
widenloop lda objpos+1,x
         sta $d001,x
         lda objpos,x
         asl a
         ror $d010
         sta $d000,x
         inx
         inx
         cpx #$10
         bne widenloop
         rts

;Animate sprites to sprite pointers

animate  lda animdelay
         cmp #4
         beq doanim
         inc animdelay
         rts
doanim   lda #$00
         sta animdelay
         ldx animpointer
         lda playerframe,x
         sta player
         lda playerdeadframe,x
         sta playerdead
         lda cloudframe,x
         sta cloud
         lda rainframe,x
         sta rain
         lda lightningframe,x
         sta lightning
         lda struckframe,x
         sta playerstruck
         inx
         cpx #$04
         beq resetframes
         inc animpointer
         rts
resetframes ldx #$00
         stx animpointer
         rts

;In game background animation

animatebg
         lda bganimdelay
         cmp #2
         beq dobganim
         inc bganimdelay
         rts
dobganim lda #$00
         sta bganimdelay
         lda $2b2f
         pha

         ;Raindrops

         ldx #$0f
shiftdown lda $2b20-1,x
         sta $2b20,x
         dex
         bpl shiftdown
         pla
         sta $2b20

         ;Flowing water

         ldx #$00
animwater lda $2b30,x
         asl a
         rol $2b30,x
         asl a
         rol $2b30,x
         inx
         cpx #8
         bne animwater
         rts

;Player movement and properties

playercontrol
         lda #4 ;Left
         bit $dc00
         bne notleftcontrol

         ;Animate and move player left

         lda player
         sta $07f8
         lda objpos
         sec
         sbc #2
         cmp #$0c
         bcs leftpos
         lda #$0c
leftpos  sta objpos
         rts

notleftcontrol
         lda #8 ;Right
         bit $dc00
         bne norightcontrol

         ;Animate and move player right

         lda player
         sta $07f8
         lda objpos
         clc
         adc #2
         cmp #$9e
         bcc rightpos
         lda #$9e
rightpos
         sta objpos
norightcontrol
         rts

;Move cloud, first check for its
;direction it can move before actually
;moving it. Once it reaches the edge of
;the screen. Swing it back again.

movecloud
         lda clouddir
         cmp #1
         bne cloudleft

         ;Cloud dir = 1 - shift right

cloudright
         lda objpos+2
         clc
speedsm1 adc cloudspeed
         cmp #$94 ;max position right
         bcc noswitch1
         lda #0
         sta clouddir
         rts
noswitch1
         sta objpos+2
         rts

         ;Cloud dir = 0 - shift left

cloudleft
         lda objpos+2
         sec
speedsm2 sbc cloudspeed
         cmp #$0c ;max position left
         bcs noswitch2
         lda #1
         sta clouddir
         rts
noswitch2
         sta objpos+2
         rts


;Test cloud spitting

cloudspitting

         jsr testspit
         jsr testdrop1
         jsr testdrop2
         jsr testdrop3
         jsr testdrop4
         jsr testdrop5
         jsr testdrop6
         rts

;Test spit - Times duration and selects
;which sprite should be fired out of
;the cloud

testspit lda left
         cmp #$30
         bne testformore
         lda left+1
         cmp #$30
         beq stopspit
testformore lda cloudtimer
         cmp cloudduration
         beq dospit
         inc cloudtimer
         rts

;Limit reached, cloud can stop spitting
;Check all rain/lightning is offset
;before confirming level complete

stopspit ldx #$00
chkloop  lda objpos+4,x
         cmp #$00
         bne notoffset
         inx
         inx
         cpx #$0c
         bne chkloop
         jmp levelcomplete
notoffset
         rts

;Level is complete.

levelcomplete

         jmp levelclear

dospit   lda #$00
         sta cloudtimer
         ldx dropcounter
         lda posxlo,x
         sta objstorex+1
         lda posxhi,x
         sta objstorex+2
         lda posylo,x
         sta objstorey+1
         lda posyhi,x
         sta objstorey+2
         lda frametypelo1,x
         sta framesm1+1
         sta framesm3+1
         lda frametypelo2,x
         sta framesm1+2
         sta framesm3+2
         lda frametypehi1,x
         sta framesm2+1
         sta framesm4+1
         lda frametypehi2,x
         sta framesm2+2
         sta framesm4+2
         lda colourlo,x
         sta colsm1+1
         sta colsm2+1
         lda colourhi,x
         sta colsm1+2
         sta colsm2+2
         inx
         cpx #posend-posstart
         beq resetpostbl
         inc dropcounter
         jsr subtractdrops
         jsr selectnext
         rts
resetpostbl
         ldx #$00
         stx dropcounter
         jsr selectnext
         rts

;Count down the number of drops+bolts
;released.

subtractdrops
         dec left+1
         lda left+1
         cmp #$2f
         bne dropsok
         lda #$39
         sta left+1
         dec left
dropsok  rts


;Select next object to be released from
;the cloud.

selectnext

;Select the object type to launch
;and select the frame in which it
;should be stored to

         ldx objectcounter
         lda behaviour,x
         sta objectfound
         inx
         cpx #100
         beq resetobjtable
         inc objectcounter
         jmp spawn
         rts

resetobjtable

         ldx #$00
         stx objectcounter

         ;Spawn the new object
spawn
         lda objectfound
         cmp #1
         beq lightningform
         lda #rainsfx
         jsr musicinit
         jmp rainform

lightningform

         lda #stormsfx
         jsr musicinit
         ldx #$00
         stx thunderpointer

         lda #<lightning
framesm1 sta dropanim1+1
         lda #>lightning
framesm2 sta dropanim1+2
         lda #$07
colsm1   sta dropcol1+1
         jsr position
         rts

rainform lda #<rain
framesm3 sta dropanim1+1
         lda #>rain
framesm4 sta dropanim1+2
         lda #$0e
colsm2   sta dropcol1+1
         jsr position
         rts

         ;Update object position to
         ;cloud position
position
         lda objpos+2
objstorex
         sta objpos+4
         lda objpos+3
objstorey
         sta objpos+5
         rts

;Dropping sprites - rain / lightning
;bolts. Set animation type and sprite
;colours.

;Test drop 1 - HW SPRITES 2-7

testdrop1
dropanim1 lda rain
         sta $07fa
dropcol1 lda #$0e
         sta $d029

         lda objpos+5
         clc
         adc dropspeed
         cmp #$fa ;Out of screen
         bcc posdrop1

         ;Test if object is rain

         lda $07fa
         cmp rainframe
         beq israin1
         cmp rainframe+1
         beq israin1
         cmp rainframe+2
         beq israin1
         jmp skipflow1
israin1
         lda objpos+4
         cmp #$00
         bne floodrisk1

         jmp skipflow1
floodrisk1

         jsr addflood


skipflow1
         lda #$00
         sta objpos+4
posdrop1
         sta objpos+5

         rts

testdrop2
dropanim2 lda rain
         sta $07fb
dropcol2 lda #$0e
         sta $d02a
         lda objpos+7
         clc
         adc dropspeed
         cmp #$fa
         bcc posdrop2

         lda $07fb
         cmp rainframe
         beq israin2
         cmp rainframe+1
         beq israin2
         cmp rainframe+2
         beq israin2
         jmp skipflow2
israin2
         lda objpos+6
         cmp #$00
         bne floodrisk2
         jmp skipflow2

floodrisk2 jsr addflood


skipflow2
         lda #$00
         sta objpos+6
posdrop2
         sta objpos+7
         rts

testdrop3
dropanim3
         lda rain
         sta $07fc
dropcol3 lda #$0e
         sta $d02b
         lda objpos+9
         clc
         adc dropspeed
         cmp #$fa
         bcc posdrop3

         lda $07fc
         cmp rainframe
         beq israin3
         cmp rainframe+1
         beq israin3
         cmp rainframe+2
         beq israin3
         jmp skipflow3
israin3

         lda objpos+8
         cmp #$00
         bne floodrisk3
         jmp skipflow3
floodrisk3 jsr addflood


skipflow3
         lda #$00
         sta objpos+8
posdrop3
         sta objpos+9
         rts

testdrop4
dropanim4
         lda rain
         sta $07fd
dropcol4 lda #$0e
         sta $d02c
         lda objpos+11
         clc
         adc dropspeed
         cmp #$fa
         bcc posdrop4

         lda $07fd
         cmp rainframe
         beq israin4
         cmp rainframe+1
         beq israin4
         cmp rainframe+2
         beq israin4
         jmp skipflow4
israin4
         lda objpos+10
         cmp #$00
         bne floodrisk4
         jmp skipflow4
floodrisk4 jsr addflood

skipflow4
         lda #$00
         sta objpos+10
posdrop4
         sta objpos+11
         rts

testdrop5
dropanim5
         lda rain
         sta $07fe
dropcol5 lda #$0e
         sta $d02d
         lda objpos+13
         clc
         adc dropspeed
         cmp #$fa
         bcc posdrop5

         lda $07fe
         cmp rainframe
         beq israin5
         cmp rainframe+1
         beq israin5
         cmp rainframe+2
         beq israin5
         jmp skipflow5
israin5
         lda objpos+12
         cmp #$00
         bne floodrisk5

         jmp skipflow5
floodrisk5 jsr addflood

skipflow5
         lda #$00
         sta objpos+12
posdrop5
         sta objpos+13
         rts

testdrop6
dropanim6
         lda rain
         sta $07ff
dropcol6 lda #$0e
         sta $d02e

         lda objpos+15
         clc
         adc dropspeed
         cmp #$fa
         bcc posdrop6

         lda $07ff
         cmp rainframe
         beq israin6
         cmp rainframe+1
         beq israin6
         cmp rainframe+2
         beq israin6
         jmp skipflow6
israin6
         lda objpos+14

         cmp #$00
         bne floodrisk6
         jmp skipflow6
floodrisk6 jsr addflood

skipflow6
         lda #$00
         sta objpos+14
posdrop6
         sta objpos+15
         rts

;Add flood count to cause the river bank
;to rise.

addflow
addflood inc flowcount
         lda #floodsfx
         jsr musicinit
         rts

;Check flow count - This basically
;fills up the water flow every time
;a water drop reaches the bottom of
;the game screen

checkflowcount

         lda flowcount
         cmp #$00
         bne notflood0
         jmp flood0
notflood0
         cmp #$01
         bne notflood1
         jmp flood1
notflood1
         cmp #$02
         bne notflood2
         jmp flood2
notflood2 cmp #$03
         bne notflood3
         jmp flood3
notflood3 cmp #$04
         bne notflood4
         jmp flood4
notflood4
         cmp #$05
         bne notflood5
         jmp flood5
notflood5
         rts

;Flood 0 - No flood

flood0   ldx #$00
copyok   lda gamemap+$02e8,x
         sta $06e8,x
         lda gamecol+$02e8,x
         sta $dae8,x
         inx
         bne copyok
         lda #$0e
         sta $db1e
         lda #$0d
         sta $dbe7
         rts

;Flooding - Start flow

flood1   lda #<floodchar1
         sta floodpos+1
         lda #>floodchar1
         sta floodpos+2
         lda #<floodcol1
         sta colpos+1
         lda #>floodcol1
         sta colpos+2
         jmp drawfloodpos

flood2   lda #<floodchar2
         sta floodpos+1
         lda #>floodchar2
         sta floodpos+2
         lda #<floodcol2
         sta colpos+1
         lda #>floodcol2
         sta colpos+2
         jmp drawfloodpos

flood3   lda #<floodchar3
         sta floodpos+1
         lda #>floodchar3
         sta floodpos+2
         lda #<floodcol3
         sta colpos+1
         lda #>floodcol3
         sta colpos+2
         jmp drawfloodpos

flood4   lda #<floodchar4
         sta floodpos+1
         lda #>floodchar4
         sta floodpos+2
         lda #<floodcol4
         sta colpos+1
         lda #>floodcol4
         sta colpos+2
         jmp drawfloodpos

flood5   lda #<floodchar5
         sta floodpos+1
         lda #>floodchar5
         sta floodpos+2
         lda #<floodcol5
         sta colpos+1
         lda #>floodcol5
         sta colpos+2
         jsr drawfloodpos
         jmp losealife

drawfloodpos
         ldx #$00
floodloop
         lda #$66
floodpos sta $07c0+1,x
         lda #$0e
colpos   sta $dbc0+1,x
         inx
         cpx #$26
         bne floodloop
         rts

;Lose a life sequence ... The player
;is tossed off the screen.

losealife
         lda #deadjingle
         jsr musicinit
         lda #$01
         sta $d015
         lda #$00
         sta $03
sinkloop
         lda #0
         sta synctimer
         cmp synctimer
         beq *-3
         jsr widenarea
         jsr animate
         jsr animatebg
         lda playerdead
         sta $07f8
         lda objpos+1
         clc
         adc #1
         cmp #$fe
         bcc placeplayer
         lda #$00
         sta objpos+0
placeplayer
         sta objpos+1
         inc $03
         lda $03
         cmp #$a0
         beq timeout
         jmp sinkloop
timeout  jmp out

;Game sprite to sprite collision

gamecollision
         jsr playread
         jsr maincoll
         rts
playread

         lda objpos
         sec
         sbc #$06
         sta collision
         clc
         adc #$0c
         sta collision+1
         lda objpos+1
         sec
         sbc #$0c
         sta collision+2
         clc
         adc #$18
         sta collision+3
         rts
maincoll
         jsr spr0tospr2
         jsr spr0tospr3
         jsr spr0tospr4
         jsr spr0tospr5
         jsr spr0tospr6
         jsr spr0tospr7
         rts

;Manual sprite/sprite collision checks
;First checks if sprite is in range of
;the X/Y Co-ordinates of the player
;then checks sprite animation lobyte
;anim values

spr0tospr2
         lda objpos+4
         cmp collision
         bcc spr2ok
         cmp collision+1
         bcs spr2ok
         lda objpos+5
         cmp collision+2
         bcc spr2ok
         cmp collision+3
         bcs spr2ok
         lda dropanim1+1
         cmp #<rain
         beq collect1
         cmp #<lightning
         beq death1
spr2ok   rts
collect1 lda #$00
         sta objpos+4
         jmp collect
death1   jmp struckscene

spr0tospr3 lda objpos+6
         cmp collision
         bcc spr3ok
         cmp collision+1
         bcs spr3ok
         lda objpos+7
         cmp collision+2
         bcc spr3ok
         cmp collision+3
         bcs spr3ok
         lda dropanim2+1
         cmp #<rain
         beq collect2
         cmp #<lightning
         beq death2
spr3ok   rts
collect2 lda #$00
         sta objpos+6
         jmp collect
death2   jmp struckscene

spr0tospr4
         lda objpos+8
         cmp collision
         bcc spr4ok
         cmp collision+1
         bcs spr4ok
         lda objpos+9
         cmp collision+2
         bcc spr4ok
         cmp collision+3
         bcs spr4ok
         lda dropanim3+1
         cmp #<rain
         beq collect3
         cmp #<lightning
         beq death3
spr4ok   rts
collect3 lda #$00
         sta objpos+8
         jmp collect
death3   jmp struckscene

spr0tospr5
         lda objpos+10
         cmp collision
         bcc spr5ok
         cmp collision+1
         bcs spr5ok
         lda objpos+11
         cmp collision+2
         bcc spr5ok
         cmp collision+3
         bcs spr5ok
         lda dropanim4+1
         cmp #<rain
         beq collect4
         cmp #<lightning
         beq death5
spr5ok   rts
collect4 lda #$00
         sta objpos+10
         jmp collect
death4   jmp struckscene

spr0tospr6
         lda objpos+12
         cmp collision
         bcc spr6ok
         cmp collision+1
         bcs spr6ok
         lda objpos+13
         cmp collision+2
         bcc spr6ok
         cmp collision+3
         bcs spr6ok
         lda dropanim5+1
         cmp #<rain
         beq collect5
         cmp #<lightning
         beq death5
spr6ok   rts
collect5 lda #$00
         sta objpos+12
         jmp collect
death5   jmp struckscene

spr0tospr7

         lda objpos+14
         cmp collision
         bcc spr7ok
         cmp collision+1
         bcs spr7ok
         lda objpos+15
         cmp collision+2
         bcc spr7ok
         cmp collision+3
         bcs spr7ok
         lda dropanim6+1
         cmp #<rain
         beq collect6
         cmp #<lightning
         beq death6
spr7ok   rts
collect6 lda #$00
         sta objpos+14
         jmp collect
death6   jmp struckscene

;Collect subroutine - A giant rain drop
;has been picked up. Award points for
;every drop picked. Also play the
;collect sfx.

collect
         lda #collectsfx
         jsr musicinit
         inc score+3
         ldx #$03
scloop   lda score,x
         cmp #$3a
         bne scoreok
         lda #$30
         sta score,x
         inc score-1,x
scoreok
         dex
         bpl scloop

         rts

;The player has been struck by lightning
;create the death animation

struckscene
         lda #deadjingle
         jsr musicinit
         lda #$07
         sta $d027 ;Colour
         lda #$00
         sta animdelay
         sta animpointer
         lda #$01
         sta $d015

struckloop
         lda #0
         sta synctimer
         cmp synctimer
         beq *-3
         jsr widenarea
         jsr animate
         jsr animatebg
         lda playerstruck
         sta $07f8

         inc $03
         lda $03
         cmp #$a0
         beq out
         jmp struckloop

;Deduct a life from the lives pointer
;if lives = 0 - process GAME OVER

out      dec lives
         jsr maskpanel
         lda lives
         cmp #$30
         beq gameover
         jmp gamereset

;Display GAME OVER text

gameover lda #$00
         sta $d015
         jsr calchiscore
         lda #gameoverjingle
         jsr musicinit
         ldx #$00
copyovertext
         lda txtgameover,x
         sta $059f,x
         lda #$02
         sta $d99f,x
         inx
         cpx #textend-textstart
         bne copyovertext
         lda #0
         sta firebutton
         jsr maskpanel
         jmp gameoverloop


;Mask the score panel on to the screen

maskpanel ldx #$00
copyscore lda score,x
         sta $0400+$06,x
         lda hiscore,x
         sta $0400+$22,x
         inx
         cpx #6
         bne copyscore

         ;mask level, lives and drops
         ;remaining to screen

         lda level
         sta $0400+$13
         lda level+1
         sta $0400+$14
         lda lives
         sta $0400+$18
         lda left
         sta $0400+$1c
         lda left+1
         sta $0400+$1d
         rts

;Level is clear, show WELL DONE text.
;Make the player walk out of the screen
;then award bonus - depending on the
;flow count value.

levelclear
         lda #1
         jsr musicinit
         lda #1
         sta $d015
         lda #$00
         sta $03

         ldx #$00
showwelldone
         lda txtwelldone,x
         sta $059f,x
         lda #$05
         sta $d99f,x
         inx
         cpx #$09
         bne showwelldone

bonusloop1
         lda #0
         sta synctimer
         cmp synctimer
         beq *-3
         jsr widenarea
         jsr animate
         jsr animatebg
         lda player
         sta $07f8
         lda objpos
         clc
         adc #1
         cmp #$ba
         bcc playpos
         lda #$00
         sta objpos+1
playpos  sta objpos
         inc $03
         lda $03
         cmp #$a0
         beq bonus2
         jmp bonusloop1
bonus2   lda #$00
         sta $03
bonusloop2 lda #$00
         sta synctimer
         cmp synctimer
         beq *-3
         jsr widenarea
         jsr animatebg
         jsr maskpanel
         inc $03
         lda $03
         cmp #$10
         bne bonusloop2
         lda #$00
         sta $03

         ;1,000 points per bonus

         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         jsr collect
         lda flowcount
         cmp #$06
         beq stopbonus
         inc flowcount
         jmp bonusloop2
stopbonus
         inc levelcount
         inc level+1
         lda level+1
         cmp #$3a
         bne levelok
         lda #$30
         sta level+1
         inc level
levelok
         jmp gamereset

;---------------------------------------
;End screen. This will just display a
;well done screen. Also calculate the
;final score the player achieved playing
;the game. (Just like with the game over
;screen. Keep game IRQ in tact.
;---------------------------------------

endsequence
         sei

         lda #$00
         sta $d015
         ldx #$00
copyendtext
         lda endmap,x
         sta $0400,x
         lda endmap+$0100,x
         sta $0500,x
         lda endmap+$0200,x
         sta $0600,x
         lda endmap+$02e8,x
         sta $06e8,x
         lda endcol,x
         sta $d800,x
         lda endcol+$0100,x
         sta $d900,x
         lda endcol+$0200,x
         sta $da00,x
         lda endcol+$02e8,x
         sta $dae8,x
         inx
         bne copyendtext
         ldx #$00
remaskcolour lda gamecol,x
         sta $d800,x
         inx
         cpx #$28
         bne remaskcolour

         lda #$00
         sta firebutton
         jsr calchiscore
         ldx #<gameirq
         ldy #>gameirq
         stx $0314
         sty $0315
         lda #$7f
         sta $dc0d
         sta $dd0d
         lda #$1b
         sta $d011
         lda #$1a
         sta $d018
         lda #$08
         sta $d016
         lda #$01
         sta $d01a
         lda #titlemus
         jsr musicinit
         cli
         jsr maskpanel

gameoverloop
         lda #$00
         sta synctimer
         cmp synctimer
         beq *-3
         jsr animatebg
         lda $dc00
         lsr a
         lsr a
         lsr a
         lsr a
         lsr a
         bit firebutton
         ror firebutton
         bmi gameoverloop
         bvc gameoverloop
         jmp titlescreen

calchiscore
         lda score
         sec
         lda hiscore+5
         sbc score+5
         lda hiscore+4
         sbc score+4
         lda hiscore+3
         sbc score+3
         lda hiscore+2
         sbc score+2
         lda hiscore+1
         sbc score+1
         lda hiscore
         sbc score

         bpl nohiscore

         ldx #$00
hiloop   lda score,x
         sta hiscore,x
         inx
         cpx #$06
         bne hiloop
nohiscore
         rts

;Game'S IRQ routine

gameirq  inc $d019
         lda $dc0d
         sta $dd0d
         lda #$fa
         sta $d012
         lda #$01
         sta synctimer
         jsr pnplayer
         jmp $ea7e

;---------------------------------------
;IRQ kill routine - for various events
;---------------------------------------
killirqs sei
         ldx #$31
         ldy #$ea
         stx $0314
         sty $0315
         lda #$81
         sta $dc0d
         sta $dd0d
         lda #$00
         sta $d019
         sta $d01a
         ldx #$00
clearscreen
         lda #$20
         sta $0400,x
         sta $0500,x
         sta $0600,x
         sta $06e8,x
         lda #$00
         sta $d800,x
         sta $d900,x
         sta $da00,x
         sta $dae8,x
         inx
         bne clearscreen
         lda #$00
         sta $d015
         ldx #$00
silence  lda #$00
         sta $d400,x
         inx
         cpx #$18
         bne silence
         rts


;---------------------------------------
;In game pointers
;---------------------------------------

;Flow count

flowcount

         .byte 0

;Level counter

levelcount .byte 0

;Thunder/ Rain table cycle counter

thunderpointer .byte 0
objectcounter .byte 0
objectfound .byte 0

cloudspeed .byte 1

;Rain/Lightning Level drop speed

dropspeed .byte 1

;Sprite frame storage

animdelay .byte 0
animpointer .byte 0

;Panel pointers (score, lives, etc.)

score    .byte $30,$30,$30,$30,$30,$30
hiscore  .byte $30,$30,$30,$30,$30,$30
level    .byte $30,$31
left     .byte $33,$30
lives    .byte $33

;Background anim pointers

bganimdelay .byte 0

;Sprite frames storage pointers

player   .byte $00
playerdead .byte $00
playerstruck .byte $00
cloud    .byte $00
rain     .byte $00
lightning .byte $00

;Cloud properties

clouddir .byte 0  ;Direction
cloudtimer .byte 0;Timer set
cloudduration .byte $80
dropcounter .byte 0

;Default sprite frames and colour

defaultframe
         .byte $80,$87,$88,$88
         .byte $88,$88,$88,$88

defaultcols
         .byte $0a,$0f,$0e,$0e
         .byte $0e,$0e,$0e,$0e

;Custom sprite position storage (X/Y)

objpos   .byte $00,$00,$00,$00
         .byte $00,$00,$00,$00
         .byte $00,$00,$00,$00
         .byte $00,$00,$00,$00

;Collision table (Box -x,-y,+x,+y)

collision
         .byte $00,$00,$00,$00

;Game sprite starting position

startpos .byte $56,$cd ;Player
         .byte $56,$40 ;Raincloud
         .byte $00,$00 ;Rest = 0
         .byte $00,$00
         .byte $00,$00
         .byte $00,$00
         .byte $00,$00
         .byte $00,$00

;Sprite frames

playerframe
         .byte $80,$81,$80,$81
playerdeadframe
         .byte $82,$83,$82,$83
struckframe
         .byte $84,$85,$86,$85
cloudframe
         .byte $87,$87,$87,$87
rainframe
         .byte $88,$89,$8a,$89
lightningframe
         .byte $8b,$8c,$8d,$8c

;Sprite position table low + hi byte
;for positioning the cloud spit

posxlo
posstart
         .byte <objpos+4
         .byte <objpos+6
         .byte <objpos+8
         .byte <objpos+10
         .byte <objpos+12
         .byte <objpos+14
posend
posxhi

xposhi
         .byte >objpos+4
         .byte >objpos+6
         .byte >objpos+8
         .byte >objpos+10
         .byte >objpos+12
         .byte >objpos+14

posylo   .byte <objpos+5
         .byte <objpos+7
         .byte <objpos+9
         .byte <objpos+11
         .byte <objpos+13
         .byte <objpos+15

posyhi   .byte >objpos+5
         .byte >objpos+7
         .byte >objpos+9
         .byte >objpos+11
         .byte >objpos+13
         .byte >objpos+15

frametypelo1
         .byte <dropanim1+1
         .byte <dropanim2+1
         .byte <dropanim3+1
         .byte <dropanim4+1
         .byte <dropanim5+1
         .byte <dropanim6+1

frametypelo2
         .byte >dropanim1+1
         .byte >dropanim2+1
         .byte >dropanim3+1
         .byte >dropanim4+1
         .byte >dropanim5+1
         .byte >dropanim6+1

frametypehi1
         .byte <dropanim1+2
         .byte <dropanim2+2
         .byte <dropanim3+2
         .byte <dropanim4+2
         .byte <dropanim5+2
         .byte <dropanim6+2

frametypehi2
         .byte >dropanim1+2
         .byte >dropanim2+2
         .byte >dropanim3+2
         .byte >dropanim4+2
         .byte >dropanim5+2
         .byte >dropanim6+2
colourlo
         .byte <dropcol1+1
         .byte <dropcol2+1
         .byte <dropcol3+1
         .byte <dropcol4+1
         .byte <dropcol5+1
         .byte <dropcol6+1
colourhi
         .byte >dropcol1+2
         .byte >dropcol2+2
         .byte >dropcol3+2
         .byte >dropcol4+2
         .byte >dropcol5+2
         .byte >dropcol6+2

;Text pointers

;GET READY TEXT (formed as byte values)

txtgetready
textstart
         .byte $07,$05,$14,$20
         .byte $12,$05,$01,$04
         .byte $19
textend

txtwelldone
         .byte $17,$05,$0c,$0c
         .byte $20,$04,$0f,$0e
         .byte $05

txtgameover
         .byte $07,$01,$0d,$05
         .byte $20,$0f,$16,$05
         .byte $12

         ;50 bytes that selects rain
         ;drop or lightning bolt

behaviour
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
behaviourend

;Level properties

         ;Game drop speed

dropspeedtable
         .byte 2,2,2,2,2,2,2,2
         .byte 3,3,3,3,3,3,3,3

levelinterval

         ;Duration before next drop

         .byte $28,$28,$28,$28
         .byte $18,$18,$18,$18
         .byte $28,$28,$28,$28
         .byte $18,$18,$18,$18

dropamountable

         ;First digit on drop counter
         ;only! - Second digit always
         ;0

         .byte $33,$33,$34,$34
         .byte $35,$35,$36,$36
         .byte $33,$33,$34,$34
         .byte $35,$35,$36,$36

;Level table, that selects specific
;object to spawn from cloud.
;
;0 = rain drop
;1 = lightning bolt

level1
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0

level2
         .byte 0,0,0,0,1,0,0,0,0,1
         .byte 0,0,0,0,1,0,0,0,0,1
         .byte 0,0,0,0,1,0,0,0,0,1
         .byte 0,0,0,0,1,0,0,0,0,1
         .byte 0,0,0,0,1,0,0,0,0,1

level3   .byte 0,0,0,1,0,0,0,1,0,0
         .byte 0,1,0,0,0,1,0,0,0,1
         .byte 0,0,0,1,0,0,0,1,0,0
         .byte 0,0,1,0,0,0,1,0,0,0
         .byte 1,0,0,0,1,0,0,0,1,0

level4   .byte 1,0,0,1,0,0,0,1,0,1
         .byte 0,0,0,1,0,0,1,0,0,1
         .byte 0,0,0,1,0,0,1,0,0,0
         .byte 1,0,0,0,1,0,0,1,0,1
         .byte 0,0,0,1,0,0,1,0,0,0

level5   .byte 0,1,1,0,0,1,0,1,1,0
         .byte 0,1,0,0,0,1,0,0,1,0
         .byte 0,1,0,0,1,0,0,1,0,1
         .byte 0,1,0,0,1,0,1,0,0,1
         .byte 0,1,0,0,1,0,1,0,1,0

level6   .byte 0,1,0,1,0,1,0,1,0,1
         .byte 0,0,0,0,0,1,0,0,0,0
         .byte 0,1,0,1,1,0,0,1,0,1
         .byte 0,1,0,0,0,1,0,0,1,0
         .byte 0,1,0,0,0,1,0,1,1,0

level7   .byte 1,1,1,0,0,0,0,0,1,1
         .byte 1,0,0,1,0,1,0,1,0,0
         .byte 0,0,0,1,0,1,0,0,1,1
         .byte 0,1,0,1,1,0,1,1,0,1
         .byte 1,1,0,0,0,1,0,1,0,1

level8   .byte 0,1,1,0,1,0,1,1,1,0
         .byte 0,0,1,0,0,1,0,0,1,0
         .byte 0,1,0,0,1,0,0,0,1,0
         .byte 1,0,1,1,0,0,1,0,1,0
         .byte 0,1,0,0,1,0,1,0,1,0

level9   .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0

level10  .byte 0,0,0,0,0,1,0,0,0,1
         .byte 0,0,0,0,1,0,0,0,1,0
         .byte 0,0,0,1,0,0,0,1,0,0
         .byte 0,0,1,0,0,0,1,0,0,0
         .byte 0,1,0,0,0,1,0,0,0,0

level11  .byte 1,0,0,1,0,0,0,1,0,1
         .byte 0,0,1,0,0,1,0,0,1,0
         .byte 1,0,0,1,0,0,1,0,0,1
         .byte 0,0,0,0,1,0,0,0,0,1
         .byte 1,0,0,1,0,0,0,1,0,1

level12  .byte 0,1,1,0,0,0,1,0,1,0
         .byte 1,0,0,0,1,0,1,0,1,0
         .byte 0,0,0,1,0,1,0,1,0,1
         .byte 0,1,0,1,0,0,1,0,0,1
         .byte 1,0,0,1,0,1,0,1,0,1
level13
         .byte 1,0,1,0,1,0,1,0,1,0
         .byte 0,0,0,0,1,0,1,0,1,1
         .byte 1,0,0,1,0,0,1,1,0,0
         .byte 1,1,1,1,0,0,0,0,0,1
         .byte 1,0,1,0,1,0,1,0,1,1
level14
         .byte 0,0,1,1,1,0,0,1,0,1
         .byte 1,0,1,0,1,0,1,0,0,0
         .byte 1,0,1,1,1,0,1,0,0,1
         .byte 1,0,1,0,0,1,0,1,1,0
         .byte 0,1,0,1,1,0,1,0,1,0

level15  .byte 0,0,0,0,0,0,0,0,0,0
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 0,0,0,0,0,0,0,0,0,0
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 0,0,0,0,0,0,0,0,0,0
level16
         .byte 0,1,0,1,0,1,0,1,0,1
         .byte 0,1,0,1,0,1,0,1,0,1
         .byte 0,1,0,1,0,1,0,1,0,1
         .byte 0,1,0,1,0,1,0,1,0,1
         .byte 0,1,0,1,0,1,0,1,0,1

         ;There is no level 17, but
         ;this is required to trigger
         ;the game complete sequence.
level17
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 1,1,1,1,1,1,1,1,1,1
         .byte 1,1,1,1,1,1,1,1,1,1

         ;Level tables (lo+hi byte)
leveltblo
levelstart
         .byte <level1,<level2,<level3
         .byte <level4,<level5,<level6
         .byte <level7,<level8,<level9
         .byte <level10,<level11
         .byte <level12,<level13
         .byte <level14,<level15
         .byte <level16,<level17
levelend
leveltbhi
         .byte >level1,>level2,>level3
         .byte >level4,>level5,>level6
         .byte >level7,>level8,>level9
         .byte >level10,>level11
         .byte >level12,>level13
         .byte >level14,>level15
         .byte >level16,>level17

;Finally thunder cloud colour
;table

thundercolour
thunderstart
         .byte $0f,$0f,$07,$07,$01,$01
         .byte $07,$07,$0f,$0f,$0c,$0c
thunderend

;--------------------------- END ------


;
; TORUS CLONE
;

; Code and graphics by TMR
; Music by TLF


; A quick, loose and written-from-scratch copy of the C64 demo
; Taurus 2 by Jeff "Yak" Minter.
; Coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map
; $0801 - $0fff		program code/data
; $1000 - $1fff		music
; $2000 - $21ff		character data
; $2200 - $23ff		plotter work characters
; $2400 - $2fff		sprites
; $2800 -		scrolling message

; Select an output filename
		!to "torus_clone.prg",cbm


; Pull in the binary data
		* = $1000
music		!binary "binary\never_wraps.prg",,2

		* = $2000
		!binary "binary\plain_font_8x8.chr"

		* = $2400
		!binary "binary\sprites.spr"


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $f1

; Label assignments
raster_num	= $50

sine_at_1	= $51
sine_at_2	= $52
sine_at_3	= $53
sine_at_4	= $54

sine_speed_1	= $55
sine_speed_2	= $56
sine_speed_3	= $57
sine_speed_4	= $58

scroll_x	= $59
scroll_colour	= $5a
scroll_pos	= $5b		; two bytes used

preset_count	= $5d
preset_pos	= $5e		; two bytes used

anim_timer	= $60


plot_workspace	= $2200

scroll_line	= $07c0
scroll_col_line	= scroll_line+$d400


; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point for the code
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Initialise some of our own labels
		lda #$01
		sta raster_num

; Initialise the preset system
		lda #$00
		sta sine_at_1
		lda #$20
		sta sine_at_2
		lda #$40
		sta sine_at_3
		lda #$60
		sta sine_at_4

		jsr preset_reset
		jsr preset_fetch
		lda #$00
		sta preset_count

; Set up the screen RAM for the plotter
		ldx #$00
		ldy #$00
screen_init	lda screen_data+$00,y
		sta $0400,x
		sta $04a0,x
		sta $0540,x
		sta $05e0,x
		sta $0680,x
		sta $0720,x
		lda screen_data+$04,y
		sta $0428,x
		sta $04c8,x
		sta $0568,x
		sta $0608,x
		sta $06a8,x
		sta $0748,x
		lda screen_data+$08,y
		sta $0450,x
		sta $04f0,x
		sta $0590,x
		sta $0630,x
		sta $06d0,x
		sta $0770,x
		lda screen_data+$0c,y
		sta $0478,x
		sta $0518,x
		sta $05b8,x
		sta $0658,x
		sta $06f8,x
		sta $0798,x
		iny
		cpy #$04
		bne *+$04
		ldy #$00
		inx
		cpx #$28
		bne screen_init

; Set up the colour RAM for the plotter
		ldx #$00
colour_init	txa
		lsr
		lsr
		tay
		lda screen_colour+$00,y
		sta $d800,x
		sta $d828,x
		sta $d850,x
		sta $d878,x

		lda screen_colour+$0a,y
		sta $d8a0,x
		sta $d8c8,x
		sta $d8f0,x
		sta $d918,x

		lda screen_colour+$14,y
		sta $d940,x
		sta $d968,x
		sta $d990,x
		sta $d9b8,x

		lda screen_colour+$1e,y
		sta $d9e0,x
		sta $da08,x
		sta $da30,x
		sta $da58,x

		lda screen_colour+$28,y
		sta $da80,x
		sta $daa8,x
		sta $dad0,x
		sta $daf8,x

		lda screen_colour+$32,y
		sta $db20,x
		sta $db48,x
		sta $db70,x
		sta $db98,x
		inx
		cpx #$28
		bne colour_init

; Set up the scroller
		ldx #$00
scroll_init	lda #$20
		sta $07c0,x
		lda #$01
		sta $dbc0,x
		inx
		cpx #$28
		bne scroll_init

; Reset the scroller
		jsr scroll_reset

; Set up the music driver
		ldx #$00
		txa
		tay
		jsr music+$00


; Restart the interrupts
		cli

; Infinite loop - all of the code is executing on the interrupt
		jmp *


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num
		cmp #$02
		bne *+$05
		jmp irq_rout2


; Raster split 1
irq_rout1	lda #$00
		sta $d020
		sta $d021

		lda #$00
		sta $d016
		lda #$18
		sta $d018

		lda #$ff
		sta $d015
		sta $d01c

; Position the hardware sprites
		ldx #$00
		ldy #$00
set_sprites	lda sprite_x,x
		clc
		adc #$6c
		sta $d000,y
		lda sprite_y,x
		clc
		adc #$48
		sta $d001,y
		lda sprite_cols,x
		sta $d027,x
		lda sprite_dps,x
		sta $07f8,x
		iny
		iny
		inx
		cpx #$08
		bne set_sprites

		lda #$0b
		sta $d025
		lda #$01
		sta $d026

; Move scrolling message
		ldx scroll_x
		inx
		cpx #$04
		bne scr_xb

; Move the text line
		ldx #$00
scroll_mover	lda scroll_line+$01,x
		sta scroll_line+$00,x
		lda scroll_col_line+$01,x
		sta scroll_col_line+$00,x
		inx
		cpx #$26
		bne scroll_mover

; Copy a new character to the scroller
		ldy #$00
scroll_mread	lda (scroll_pos),y
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

scroll_okay	cmp #$80
		bcc scroll_okay_2
		and #$0f
		sta scroll_colour
		lda #$20

scroll_okay_2	sta scroll_line+$26
		lda scroll_colour
		sta scroll_col_line+$26

; Nudge the scroller onto the next character
		inc scroll_pos+$00
		bne *+$04
		inc scroll_pos+$01

		ldx #$00
scr_xb		stx scroll_x

; Update the presets if need be
		ldx preset_count
		inx
		cpx #$f1
		bne no_new_preset
		jsr preset_fetch
		ldx #$00
no_new_preset	stx preset_count

; Play the music
		jsr music+$03

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2	lda scroll_x
		and #03
		asl
		eor #$07
		sta $d016

; Remove the current points from the character data
!set plot_count=$00
!do {
		lda plot_x+plot_count
		and #$f8
		asl
		asl
		clc
		adc plot_y+plot_count
		tay
		lda #$00
		sta plot_workspace,y

		!set plot_count=plot_count+$01
} until plot_count=$08

; Update the curve counters to move the sprites
		ldx #$00
sine_update	lda sine_at_1,x
		clc
		adc sine_speed_1,x
		sta sine_at_1,x
		inx
		cpx #$04
		bne sine_update

; Work out the sprite positions for this frame
		ldx #$00
		ldy sine_at_1
sine_x_gen_1	lda sprite_sinus,y
		sta sprite_x,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne sine_x_gen_1

		ldx #$00
		ldy sine_at_2
sine_x_gen_2	lda sprite_sinus,y
		clc
		adc sprite_x,x
		sta sprite_x,x
		lsr
		lsr
		sta plot_x,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne sine_x_gen_2

		ldx #$00
		ldy sine_at_3
sine_y_gen_1	lda sprite_sinus,y
		sta sprite_y,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne sine_y_gen_1

		ldx #$00
		ldy sine_at_4
sine_y_gen_2	lda sprite_sinus,y
		clc
		adc sprite_y,x
		sta sprite_y,x
		lsr
		lsr
		sta plot_y,x
		tya
		clc
		adc #$20
		tay
		inx
		cpx #$08
		bne sine_y_gen_2

; Plot new points to the character data
!set plot_count=$00
!do {
		lda plot_x+plot_count
		and #$f8
		asl
		asl
		clc
		adc plot_y+plot_count
		tay
		lda plot_x+plot_count
		and #$07
		tax
		lda plot_workspace,y
		ora plot_offsets,x
		sta plot_workspace,y

		!set plot_count=plot_count+$01
} until plot_count=$08

; Update sprite animations
		ldx anim_timer
		inx
		cpx #$04
		bne at_xb

		ldx #$00
anim_update	lda sprite_dps,x
		clc
		adc #$01
		cmp #$99
		bne *+$04
		lda #$90
		sta sprite_dps,x
		inx
		cpx #$08
		bne anim_update

		ldx #$00
at_xb		stx anim_timer

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012


; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti

; Subroutine to reset the scrolling message
scroll_reset	lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01
		rts

; Subroutines for reading sprite preset data
preset_fetch	ldx #$00

pf_loop		ldy #$00
preset_mread	lda (preset_pos),y
		cmp #$80
		bne preset_okay
		jsr preset_reset
		jmp preset_mread

preset_okay	sta sine_speed_1,x

		inc preset_pos+$00
		bne *+$04
		inc preset_pos+$01
		inx
		cpx #$04
		bne pf_loop

		rts

preset_reset	lda #<preset_data
		sta preset_pos+$00
		lda #>preset_data
		sta preset_pos+$01
		rts


; Sprite position tables
sprite_x	!byte $40,$48,$50,$58,$60,$68,$70,$78
sprite_y	!byte $40,$48,$50,$58,$60,$68,$70,$78
sprite_cols	!byte $0a,$08,$05,$04,$0a,$08,$05,$04
sprite_dps	!byte $90,$91,$92,$93,$94,$95,$96,$97

; Plotter position tables
plot_x		!byte $00,$00,$00,$00,$00,$00,$00,$00
plot_y		!byte $00,$00,$00,$00,$00,$00,$00,$00

; Plotter fine positioning table
plot_offsets	!byte $80,$40,$20,$10,$08,$04,$02,$01

; Sine table for the movement
sprite_sinus	!byte $20,$20,$21,$22,$23,$24,$24,$25
		!byte $26,$27,$27,$28,$29,$2a,$2a,$2b
		!byte $2c,$2d,$2d,$2e,$2f,$2f,$30,$31
		!byte $31,$32,$33,$33,$34,$35,$35,$36
		!byte $36,$37,$37,$38,$38,$39,$39,$3a
		!byte $3a,$3b,$3b,$3b,$3c,$3c,$3d,$3d
		!byte $3d,$3d,$3e,$3e,$3e,$3e,$3f,$3f
		!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f

		!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
		!byte $3f,$3f,$3f,$3e,$3e,$3e,$3e,$3d
		!byte $3d,$3d,$3c,$3c,$3c,$3b,$3b,$3b
		!byte $3a,$3a,$39,$39,$38,$38,$37,$37
		!byte $36,$36,$35,$34,$34,$33,$33,$32
		!byte $31,$31,$30,$2f,$2f,$2e,$2d,$2c
		!byte $2c,$2b,$2a,$29,$29,$28,$27,$26
		!byte $26,$25,$24,$23,$23,$22,$21,$20

		!byte $1f,$1f,$1e,$1d,$1c,$1c,$1b,$1a
		!byte $19,$18,$18,$17,$16,$15,$15,$14
		!byte $13,$12,$12,$11,$10,$10,$0f,$0e
		!byte $0e,$0d,$0c,$0c,$0b,$0b,$0a,$09
		!byte $09,$08,$08,$07,$07,$06,$06,$05
		!byte $05,$04,$04,$04,$03,$03,$03,$02
		!byte $02,$02,$01,$01,$01,$01,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$01,$01,$01,$01,$02
		!byte $02,$02,$03,$03,$03,$04,$04,$05
		!byte $05,$05,$06,$06,$07,$07,$08,$08
		!byte $09,$0a,$0a,$0b,$0b,$0c,$0d,$0d
		!byte $0e,$0f,$0f,$10,$11,$11,$12,$13
		!byte $13,$14,$15,$16,$16,$17,$18,$19
		!byte $19,$1a,$1b,$1c,$1d,$1d,$1e,$1f

; Preset data for the sinus routine
preset_data	!byte $03,$02,$01,$04
		!byte $fb,$fe,$03,$06
		!byte $00,$01,$02,$03
		!byte $05,$fb,$fd,$fe
		!byte $02,$fe,$fb,$06
		!byte $00,$fb,$00,$fb
		!byte $07,$01,$03,$fe
		!byte $05,$06,$08,$04

		!byte $02,$01,$02,$01
		!byte $fc,$01,$f8,$07
		!byte $07,$05,$f9,$02
		!byte $02,$07,$05,$04
		!byte $00,$04,$05,$08
		!byte $02,$03,$01,$02
		!byte $f9,$07,$fe,$03
		!byte $02,$06,$01,$fb

		!byte $ff,$fe,$fd,$fe
		!byte $fe,$fa,$f9,$06
		!byte $fa,$04,$05,$07
		!byte $f7,$05,$fb,$03
		!byte $f8,$f9,$08,$fe
		!byte $01,$00,$05,$07
		!byte $f9,$08,$fb,$01
		!byte $f9,$08,$f9,$fd
		!byte $80

; Screen data for the plotters (tiled across and down the screen)
screen_data	!byte $40,$44,$48,$4c
		!byte $41,$45,$49,$4d
		!byte $42,$46,$4a,$4e
		!byte $43,$47,$4b,$4f

screen_colour	!byte $09,$02,$08,$0a,$0f,$0f,$0a,$08,$02,$09
		!byte $02,$08,$0a,$0f,$07,$07,$0f,$0a,$08,$02
		!byte $08,$0a,$0f,$07,$01,$01,$07,$0f,$0a,$08
		!byte $04,$0e,$03,$0d,$01,$01,$0d,$03,$0e,$04
		!byte $0b,$04,$0e,$03,$0d,$0d,$03,$0e,$04,$0b
		!byte $06,$0b,$04,$0e,$03,$03,$0e,$04,$0b,$06


; Text for the scrolling message - $00 wraps to the start and the values
; from $80 to $8f set the text colour
		* = $2800
scroll_text	!byte $85
		!scr "welcome to  ",$8d,"-=- torus clone -=-",$85,"  a loose copy of "
		!scr "the",$8f,"yak",$85,"demo  ",$87,"-=- taurus 2 -=-",$85,"  "
		!scr "which came out almost thirty years ago",$83,"in 1986!"
		!scr "      "

		!scr $8e,"code and graphics by",$83,"t.m.r",$8e
		!scr "with music composed by",$83,"t.l.f."
		!scr "      "

		!scr $8a,"goodness, it only seems like yesterday that",$87,"clonemas",$8a
		!scr "was being released...    but it was last week!    "
		!scr $88,"that was a spur of the moment ",$22,"bonus",$22," which "
		!scr "was thrown together because i had nothing better to do whilst "
		!scr "waiting for the good telly to start on",$82,"christmas day",$88
		!scr "but there wasn't a deliberate decision made to push this one out "
		!scr "on new year's eve;   the",$8a,"c64cd",$88,"release "
		!scr $22,"schedule",$22," of one demo every sort-of-fourish weeks just "
		!scr "happened to line up that way!"
		!scr "      "

		!scr $84,"one slight design tweak in this demo is that the original "
		!scr "didn't come with a scroller - i bolted one in both to occupy "
		!scr "the last line of the screen and to deliver those highly important"
		!scr $8e,"c64cd",$84,"hellos, speaking of which..."
		!scr "      "

		!scr $8c,"c64cd",$8b,"thanks-for-the-inspiration greetings go out to:    "
		!scr $82,"the harlow cracking service",$8b," -+- "
		!scr $88,"rob hubbard",$8b," -+- "
		!scr $87,"happy democoder",$8b," -+- "
		!scr $85,"stoat and tim",$8b," -+- "
		!scr $8e,"yak",$8b," -+- "
		!scr $8c,"and an ",$22,"anti-greeting",$22," to",$84,"c64hater",$8c
		!scr "as well!"
		!scr "      "

		!scr $8a,"and with that out of the way there isn't any text left, "
		!scr "so",$87,"t.m.r",$8a,"woz 'ere on the 31st of december 2015... .. .  ."
		!scr "            "

		!byte $00		; end of text marker

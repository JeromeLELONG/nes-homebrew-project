;
; example.s
; Brad Smith (rainwarrior), 4/06/2014
; http://rainwarrior.ca
;
; This is intended as an introductory example to NES programming with ca65.
; It covers the basic use of background, sprites, and the controller.
; This does not demonstrate how to use sound.
;
; This is not intended as a ready-made game. It is only a very minimal
; playground to assist getting started in NES programming. The idea here is
; to get you past the most difficult parts of a minimal NES program setup
; so that you can experiment from an almost blank slate.
;
; To use your own graphics, replace the two 4k tile banks provided.
; They are named "background.chr" and "sprite.chr".
;
; The reset and nmi routines are provided as a simple working example of
; these things. Writing these from scratch is a more advanced topic, so they
; will not be fully explained here.
;
; Under "drawing utilities" are some very primitive routines for working
; with the NES graphics. See the "main" section for examples of how to use them.
;
; Finally at the bottom you will find the "main" section that provides
; a small example program. A cursor is shown. Pressing the d-pad will move
;   - pressing the d-pad will move the cursor around the screen
;   - pressing B will draw a tile to the screen
;   - pressing A will draw several tiles to the screen
;   - pressing SELECT will reset the background
;   - holding START will demonstrate scrolling
;
; Please note that this example code is intended to be simple, not necessarily
; efficient. I have tried to avoid optimization in favour of easier to understand code.
;
; You may notice some odd behaviour when using the A button around the edges of the screen.
; I will leave it as an exercise for the curious to understand what is going on.
;

;
; iNES header
;

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;
; CHR ROM
;

.segment "TILES"
;.incbin "graphics/tileset.chr"
.incbin "graphics/virus.chr"
.incbin "graphics/sprites.chr"
;.incbin "sprite.chr"

;
; vectors placed at top 6 bytes of memory area
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; reset routine
;

.segment "CODE"
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ici activer le son
	lda #$01                ; enable pulse 1
    sta $4015
    lda #$08                ; period
    sta $4002
    lda #$02
    sta $4003
    ;lda #$bf                ; volume
	lda #00
    sta $4000
	; Fin activation du son
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
position_x_sprite_1: .res 1
index_heart_x:      .res 1
load_background: .res 1
already_moved:  .res 1
test_cmp:		.res 1
test_cmp_count:  .res 1
seed:			.res 2
cursor_x: .res 1
cursor_y: .res 1
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_count_2:    .res 1
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable
cursor_y_2: 	.res 1
a_pressed:		.res 1
cursor_x_heart: 	.res 1
cursor_y_heart: 	.res 1


.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;lda nmi_count
	;cmp #21
	;bcs :+
	; 	inc nmi_count_2
	; 	lda #0
	; 	sta already_moved
	;:

	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:

	;lda cursor_y_2
	;cmp #40
	;bcc :+
		;inc cursor_y_2
	;:
	;lda cursor_y_2
	;sta oam+(22)+0
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:

	; augmenter le volume
	lda nmi_count_2
	cmp #2
	bne :+
		lda #$01                ; enable pulse 1
    	sta $4015
    	lda #$08                ; period
    	sta $4002
    	lda #$02
    	sta $4003
    	;lda #$bf                ; volume
		lda #$bf
    	sta $4000
	:
	;

	; diminuer le volume
	lda nmi_count_2
	cmp #3
	bne :+
	 	;lda #$01                ; enable pulse 1
		lda #$00				; disable pulse 1
     	sta $4015
     	lda #$08                ; period
     	sta $4002
     	lda #$02
     	sta $4003
     	;lda #$bf                ; volume
	 	lda #$00
     	sta $4000
	:
	;


	; enregister la position x du sprite 1
	lda oam+(0*4)+3
	sta position_x_sprite_1

	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	; palettes
	lda #%10001000
	sta $2000 ; set horizontal nametable increment
	lda $2002
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta $2000
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	; enable rendering
	lda #%00011110
	sta $2001
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

;
; irq
;

.segment "CODE"
irq:
	rti

;
; drawing utilities
;

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta $2006 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
;
; main
;

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

.segment "RODATA"
example_palette:
.byte $0c,$22,$10,$32,$0c,$04,$1c,$3d,$0c,$05,$2a,$29,$0c,$14,$2c,$30

;.byte $0F,$15,$26,$37 ; bg0 purple/pink
;.byte $0F,$09,$19,$29 ; bg1 green
;.byte $0F,$01,$11,$21 ; bg2 blue
;.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow   palette sprites 1
.byte $0F,$14,$16,$34 ; sp1 purple   palette sprites 2
.byte $0F,$1B,$2B,$3B ; sp2 teal   palette sprites 3
.byte $0F,$12,$22,$32 ; sp3 marine   palette sprites 4

background:
.incbin "map/imagemap.map"
	; .byte	$91,$92,$9B,$9B,$9B,$9B,$9B,$9B
	; .byte	$9B,$9B,$9B,$9B,$9B,$9B,$9B,$9B
	; .byte	$D6,$D7,$90,$90,$90,$90,$90,$90
	; .byte	$90,$90,$90,$90,$90,$90,$90,$90
	; .byte	$90,$90,$90,$90,$90,$90,$90,$90
	; .byte	$90,$90,$90,$90,$90,$90,$90,$90
	; .byte	$90,$90,$90,$90,$90,$90,$90,$90
	; .byte	$90,$90,$90,$90,$90,$90,$90,$90

.segment "ZEROPAGE"
;cursor_x: .res 1
;cursor_y: .res 1
temp_x:   .res 1
temp_y:   .res 1
start_pressed: .res 1


.segment "CODE"
main:
	;lda #200
	;cmp #2
	;beq :+      ; comparaison égal zéro, la branche fonctionne
	;	lda #$FF
	;	sta test_cmp
	;:

	; setup 
	lda #32
	sta cursor_x_heart
	lda #10
	sta cursor_y_heart

	lda #32
	sta index_heart_x

	lda #100
	beq :+   ; si la valeur chargée dans l'accumulateur vaut plus que zéro, la branche fonctionne
		inc test_cmp_count
		lda #$FF
		sta test_cmp
	:

	ldx #0
	:
		inc test_cmp_count
		lda example_palette, X
		sta palette, X
		inx
		cpx #32
		bcc :-
	lda #1
	sta seed
	jsr setup_background
	lda #0
	sta load_background
	lda #0
	sta start_pressed
	lda #20
	sta cursor_y_2
	; center the cursor
	lda #128
	sta cursor_x
	lda #120
	sta cursor_y
	; show the screen
	jsr draw_cursor
	jsr ppu_update
	; main loop
@loop:
	;inc nmi_count_2
	lda nmi_count
	cmp #21
	beq :+
		;inc nmi_count_2
		;lda already_moved
		;cmp #0
		;beq :+
		;	inc nmi_count_2
		;:
		lda #1
		sta already_moved
	:



	lda nmi_count
	cmp #20
	beq :+
		lda #0
		sta already_moved
	:

	;lda nmi_count
	;cmp #20
	;bcs :+
	lda already_moved
	cmp #0
	beq :+
		;lda already_moved
		;cmp #0
		;bcs :+
			jsr galois16
			jsr update_y_sprite_position
			;inc nmi_count_2
			;inc nmi_count
			lda #1
			sta already_moved
		;:
	:
	;jsr galois16
	; read gamepad
	jsr gamepad_poll
	lda gamepad
	and #PAD_START
	beq :+
		jsr push_start
		jmp @draw ; start trumps everything, don't check other buttons
	:
	lda gamepad
	and #PAD_A
	beq :+
		jsr push_a
	:
		lda gamepad
	and #PAD_U
	beq :+
		jsr push_u
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr push_d
	:
	lda gamepad
	and #PAD_L
	beq :+
		jsr push_l
	:
	lda gamepad
	and #PAD_R
	beq :+
		jsr push_r
	:
	;lda cursor_y
	;cmp #20
	;bcc :+
	;	jsr update_y_sprite_position
	;:

	;lda cursor_y
	;lda cursor_x
	;cmp #20
	;bcs :+
		jsr update_x_sprite_position
	;:

@draw:
	; draw everything and finish the frame
	
	
	jsr draw_cursor
	jsr draw_cursor_2
	jsr ppu_update
	; keep doing this forever!
	jmp @loop

galois16:
	ldy #8
	lda seed+0
:
	asl        ; shift the register
	rol seed+1
	bcc :+
	eor #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
	dey
	bne :--
	sta seed+0
	cmp #0     ; reload flags
	sta cursor_y
	inc nmi_count_2
	rts

update_y_sprite_position:
	; jsr galois16
	; ldx #0
	;lda seed
	

	; dec cursor_y
	; ; Y wraps at 240
	; lda cursor_y
	; cmp #240
	; bcc :+
	; 	lda #239
	; 	sta cursor_y
	; :
	rts

update_x_sprite_position:
	dec cursor_x
	; Y wraps at 240
	lda cursor_x
	cmp #240
	bcc :+
		lda #239
		sta cursor_x
	:
	rts

draw_cursor:
	; four sprites centred around the currently selected tile
	; y position (note, needs to be one line higher than sprite's appearance)
	lda cursor_y
	sec
	sbc #5 ; Y-5
	sta oam+(0*4)+0
	sta oam+(1*4)+0
	lda cursor_y
	clc
	adc #3 ; Y+3
	sta oam+(2*4)+0
	sta oam+(3*4)+0
	; tile
	lda #0
	sta oam+(0*4)+1
	lda #1
	sta oam+(1*4)+1
	lda #16
	sta oam+(2*4)+1
	lda #17
	sta oam+(3*4)+1
	lda #17
	; attributes
	lda #%00000000 ; no flip
	sta oam+(0*4)+2
	;lda #%01000000 ; horizontal flip
	sta oam+(1*4)+2
	;lda #%10000000 ; vertical flip
	sta oam+(2*4)+2
	;lda #%11000000 ; both flip
	sta oam+(3*4)+2

	; x position
	lda cursor_x
	sec
	sbc #4 ; X-4
	sta oam+(0*4)+3
	sta oam+(2*4)+3
	lda cursor_x
	clc
	adc #4 ; X+4
	sta oam+(1*4)+3
	sta oam+(3*4)+3


draw_cursor_2:
	lda cursor_y_heart
	;lda #10   ; position Y du sprite
	sta oam+(4*4)+0
	lda #56 ; tile du sprite coeur
	sta oam+(4*4)+1 
	lda #%10000001 ; vertical flip et palette 2
	sta oam+(4*4)+2
	lda cursor_x_heart
	;lda #32 ; position X du sprite 
	sta oam+(4*4)+3


	;sta oam+(21)+1
	;sta oam+(20)+2
	;sta oam+(21)+2
	;lda #10
	;sta oam+(20)+3
	;lda #20
	;sta oam+(21)+3
	rts


setup_background:
	; first nametable, start by clearing to empty
	lda $2002 ; reset latch
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	; empty nametable
	lda #0
	ldy #30 ; 30 rows
	:
		ldx #32 ; 32 columns
		:
			sta $2007
			dex
			bne :-
		dey
		bne :--
	; set all attributes to 0
	;ldx #64 ; 64 bytes
	ldx #64 ; 64 bytes
	:
		sta $2007
		dex
		bne :-
	; fill in an area in the middle with 1/2 checkerboard
	;lda #0
	;ldy #8
	;ldx #8
	;jsr ppu_address_tile
	;sta $2007
	;lda #2
	;ldy #8
	;ldx #9
	;jsr ppu_address_tile
	;lda #2
	;sta $2007
	;pha
	;jsr ppu_update
	;pla
;@forever:
;	jmp @forever
	;lda #3
	;ldy #8
	;ldx #10
	;jsr ppu_address_tile
	;sta $2007
	ldy #8 ; start at row 8
	:
		pha ; temporarily store A, it will be clobbered by ppu_address_tile routine
		ldx #8 ; start at column 8
		jsr ppu_address_tile
		pla ; recover A
		; write a line of checkerboard
		ldx #8
	:
		stx temp_x
		ldx load_background
		lda background, x
		inc load_background
		ldx temp_x
		sta $2007
		eor #$3
		inx
		;cpx #(32-8)
		cpx #24
		bcc :-
		eor #$3
		iny
		;cpy #(32-8)
		cpy #24
		bcc :--
	; second nametable, fill with simple pattern
	
	rts

push_start:
	lda #1
	sta start_pressed
	lda #128
	sta cursor_x
	lda #120
	sta cursor_y

	;lda #4
	;ldx #8
	;ldy #8
	;jmp ppu_update_tile
	
	;lda #4
	;ldx #9
	;ldy #9
	;jmp ppu_update_tile
	
	;lda #4
	;ldx #9
	;ldy #9
	;jmp ppu_update_tile
	
	;lda #4
	;ldx #10
	;ldy #10
	;jmp ppu_update_tile
	
	;lda #4
	;ldx #11
	;ldy #11
	;jmp ppu_update_tile
	
	;lda #4
	;ldx #12
	;ldy #12
	;jmp ppu_update_tile
	
	lda #170
	ldx index_heart_x
	inc index_heart_x
	ldy #0
	jsr ppu_update_tile
	rts


push_a:
	lda #1
	sta a_pressed
	lda #60
	sta cursor_x
	lda #60
	sta cursor_y
	rts

;
; end of file
;

push_u:
	dec cursor_y_heart
	; Y wraps at 240
	lda cursor_y_heart
	cmp #240
	bcc :+
		lda #239
		sta cursor_y_heart
	:
	rts

push_d:
	inc cursor_y_heart
	; Y wraps at 240
	lda cursor_y_heart
	cmp #240
	bcc :+
		lda #0
		sta cursor_y_heart
	:
	rts

push_l:
	dec cursor_x_heart
	rts

push_r:
	inc cursor_x_heart
	rts
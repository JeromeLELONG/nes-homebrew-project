p1_x:           	.res 1
p1_x_high:      	.res 1
p1_y:           	.res 1
p1_y_high:      	.res 1
p1_speed_x:     	.res 1
p1_speed_x_high:	.res 1
p1_speed_y:     	.res 1
p1_speed_y_high:	.res 1
p1_ground:      	.res 1
p1_timer:       	.res 1
p1_state:       	.res 1
p1_dir:         	.res 1
p1_attribute:		.res 1

p2_x:           	.res 1
p2_x_high:      	.res 1
p2_y:           	.res 1
p2_y_high:      	.res 1
p2_speed_x:     	.res 1
p2_speed_y:     	.res 1
p2_ground:      	.res 1
p2_timer:       	.res 1
p2_state:       	.res 1
p2_dir:         	.res 1
p2_attribute:		.res 1

girl_x:           	.res 1
girl_y:           	.res 1
girl_dir:         	.res 1

cougar_x:          	.res 1
cougar_y:          	.res 1
cougar_dir:        	.res 1

waterfall_x:		.res 1

FT_TEMP:			.res 3 ;3 bytes in zeropage used by the library as a scratchpad

pad1:				.res 1
pad2:				.res 1
pad1_newkeys:		.res 1
pad2_newkeys:		.res 1

temp_ptr:       	.res 2
temp_ptr2:       	.res 2
pal_ptr:			.res 2
ppumask_soft:		.res 1
framedone:			.res 1
scroll_lowbit:		.res 1 ; used for wrap around

scroll_x:       	.res 1
scroll_x_spd:   	.res 1
scroll_y:       	.res 1
scroll_dir:     	.res 1
chr_bank_now:		.res 1
chr_bank:       	.res 1
chr_timer:      	.res 1
pal_cycle:      	.res 1
tempa:          	.res 1
gamestate:			.res 1
fadestate:			.res 1 ; 0 = fading in done, 1 = fading in, 2 = fading out, 3 = fading out done

oam_pos:			.res 1
animation_ptr:		.res 2
spr_flip_mask:		.res 1
spr_dir:			.res 1
spr_attribute:		.res 1
spr_y:				.res 1
spr_x:				.res 1

gametimerlow:		.res 1
gametimerhigh:		.res 1

pad1_a:				.res 1
pad2_a:				.res 1
pad1_a_pressed:		.res 1
pad2_a_pressed:		.res 1
pad1_b:				.res 1
pad2_b:				.res 1
pad1_b_pressed:		.res 1
pad2_b_pressed:		.res 1
pad1_select:		.res 1
pad2_select:		.res 1
pad1_select_pressed:.res 1
pad2_select_pressed:.res 1
pad1_start:			.res 1
pad2_start:			.res 1
pad1_start_pressed:	.res 1
pad2_start_pressed:	.res 1
pad1_up:			.res 1
pad2_up:			.res 1
pad1_down:			.res 1
pad2_down:			.res 1
pad1_left:			.res 1
pad2_left:			.res 1
pad1_right:			.res 1
pad2_right:			.res 1

; Active obj data. Each obj uses 10 (dec) bytes. Here's the structure:

; 0 = obj ID
; 1 = x
; 2 = x_high (used for off-screen positioning)
; 3 = y
; 4 = y_high (used for off-screen positioning)
; 5 = h_speed
; 6 = v_speed
; 7 = attributes (palette to use, flip bits)
; 8 = obj state (some objs have different states used by their internal logic)
; 9 = high bits: current animation frame. low bits: current frame time

; This range gives room for 16  active objs at any time (more than enough?)

; Most games don't use zero page for this, but in this game there's room and it's faster access.

; .res could be set to 1, and as long as this is the last definition it shouldn't matter,
; but setting it to a known value fires an error at compile time if zero page definitions
; overflow. So you can rely on this range to work.

objs_pos:			.res 1
OBJS:				.res $A0
; Bresenham's line drawing algorithm
; https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#All_cases

; Usage:
;
;     Move the cursor with <W> <A> <S> <D> and press <ENTER> to select the
;   pixel at the cursor location. The first selected pixel will be the start
;   of the line, and the second one its end. After selecting the end pixel
;   the line will be drawn. To reset the screen and the cursor position
;   press <R>.

; #--------------------------------------------# ;

; see memory layout in ./memory_layout.ods
; uses only zeropage

define  pixel_current_L $00 ; stores screen addrs at witch pixel will be drawn
define  pixel_current_H $01
define  pixel_current_x $02
define  pixel_current_y $03
define  pixel_cursor_x  $04
define  pixel_cursor_y  $05
define  pixel_start_x   $06
define  pixel_start_y   $07
define  pixel_end_x     $08
define  pixel_end_y     $09
define  pixel_swap_x    $0a
define  pixel_swap_y    $0b

; next pixel to be selected
;   0 -> start pixel
;   1 -> end pixel
;   2 -> both pixels selected, draw line
define  pixel_current_selection $0d

; constants used in the line algorithm
define  _dx     $10
define  _dy     $11
define  _2dx    $12
define  _2dy    $13
define  _dir_x  $14
define  _dir_y  $15
define  _abs_dx $16
define  _abs_dy $17

; variables / constants used in the line algorithm
define  _D        $20
define  _D_step_H $21 ; 2 * (dy - dx)
define  _D_step_V $22 ; 2 * (dx - dy)

define  color_current_pixel $40 ; color under the cursor
define  color_drawing       $41
define  color_cursor        $42

; ascii codes of keys
define  keycode_w     $77
define  keycode_a     $61
define  keycode_s     $73
define  keycode_d     $64
define  keycode_r     $72
define  keycode_enter $0d

; system variables
define  keycode_pressed $ff

; #--------------------------------------------# ;

init:
  ; starting cursor position
  lda #$0f
  sta pixel_cursor_x
  sta pixel_cursor_y
  ; cursor color
  lda #$0c
  sta color_cursor
  ; X register should always be 0
  ldx #$00

; #--------------------------------------------# ;

main_loop:
  jsr update_keyboard
  jsr pos_to_addr
  jsr blink_cursor_pixel

  jmp main_loop

; #--------------------------------------------# ;

blink_cursor_pixel:
  jsr copy_cursor_to_current
  ; save the color under the cursor, so it is not overwriten
  lda (pixel_current_L, X)
  sta color_current_pixel
  ; set current pixel color to cursor color
  lda color_cursor
  sta (pixel_current_L, X)
  jsr delay
  ; restore original pixel color
  lda color_current_pixel
  sta (pixel_current_L, X)
  jsr delay

  rts

; #--------------------------------------------# ;

update_keyboard:
  lda keycode_pressed

  cmp #keycode_w
  beq move_up
  cmp #keycode_a
  beq move_left
  cmp #keycode_s
  beq move_down
  cmp #keycode_d
  beq move_right
  cmp #keycode_r
  beq reset
  cmp #keycode_enter
  beq select_pixel

  jmp update_keyboard_end

move_up:
  lda pixel_cursor_y
  ; if y == 0: can't move up
  beq out_of_bound
  dec pixel_cursor_y

  jmp update_keyboard_end

move_down:
  lda pixel_cursor_y
  ; if y == 31: can't move down
  cmp #$1f
  beq out_of_bound
  inc pixel_cursor_y

  jmp update_keyboard_end

move_left:
  lda pixel_cursor_x
  ; if x == 0: can't move left
  beq out_of_bound
  dec pixel_cursor_x

  jmp update_keyboard_end

move_right:
  lda pixel_cursor_x
  ; if x == 31: can't move right
  cmp #$1f
  beq out_of_bound
  inc pixel_cursor_x

  jmp update_keyboard_end

reset:
  ; replace cursor at the middle of the screen
  lda #$0f
  sta pixel_cursor_x
  sta pixel_cursor_y
  ; no pixel selected, next start
  lda #$00
  sta pixel_current_selection

  jsr reset_screen

  jmp update_keyboard_end

; select starting / end pixel coordinates
; if both selected: draw line
select_pixel:
  ; test if start has been selected or not
  lda pixel_current_selection
  beq select_pixel_start
  jmp select_pixel_end

  select_pixel_start:
    ; copy cursor pos to start pos
    lda pixel_cursor_x
    sta pixel_start_x
    lda pixel_cursor_y
    sta pixel_start_y
    jmp select_end
  select_pixel_end:
    ; copy cursor pos to end pos
    lda pixel_cursor_x
    sta pixel_end_x
    lda pixel_cursor_y
    sta pixel_end_y
    jmp select_end
  select_end:
    ; draw red pixel under the cursor to indicate the selected pixel
    lda #$02
    sta color_drawing
    jsr copy_cursor_to_current
    jsr plot_pixel
    ; a pixel has been selected, go to next step
    inc pixel_current_selection
    ; if both start end end selected, draw line
    lda pixel_current_selection
    cmp #$02
    bne update_keyboard_end
    lda #$01
    sta color_drawing
    jsr draw_line
    lda #$00
    sta pixel_current_selection

  jmp update_keyboard_end

out_of_bound:
update_keyboard_end:
  ; resest pressed key
  lda #$00
  sta $ff
  
  rts

; #--------------------------------------------# ;

; initialize constants used by line algorithm
init_line:
  ; dx
  lda pixel_end_x
  sec
  sbc pixel_start_x
  sta _dx
  ; 2dx
  clc
  adc _dx
  sta _2dx
  ; dy
  lda pixel_end_y
  sec
  sbc pixel_start_y
  sta _dy
  ; 2dy
  clc
  adc _dy
  sta _2dy
  ; abs(dx)
  lda _dx
  bpl _dx_positive
  jsr negate
  _dx_positive:
    sta _abs_dx
  ; abs(dy)
  lda _dy
  bpl _dy_positive
  jsr negate
  _dy_positive:
    sta _abs_dy

  rts

; #--------------------------------------------# ;

init_line_horizontal:
  ; dir_y
  lda #$01
  sta _dir_y
  lda _dy
  ; if dy >= 0: continue
  bpl init_line_horizontal_continue
  lda #$ff
  sta _dir_y
  ; if dy < 0: dy = abs(dy)
  lda _dy
  jsr negate
  sta _dy
  ; if dy < 0: 2dy = abs(2dy)
  lda _2dy
  jsr negate
  sta _2dy
init_line_horizontal_continue:
  ; D
  lda _2dy
  sec
  sbc _dx
  sta _D
  ; D_step_H
  lda _dy
  sec
  sbc _dx
  sta _D_step_H
  clc
  adc _D_step_H
  sta _D_step_H
  ; y 
  lda pixel_start_y
  sta pixel_current_y
  
  rts

; #--------------------------------------------# ;

init_line_vertical:
  ; dir_x
  lda #$01
  sta _dir_x
  lda _dx
  ; if dx >= 0: continue
  bpl init_line_vertical_continue
  lda #$ff
  sta _dir_x
  ; if dx < 0: dx = abs(dx)
  lda _dx
  jsr negate
  sta _dx
  ; if dx < 0: 2dx = abs(2dx)
  lda _2dx
  jsr negate
  sta _2dx
init_line_vertical_continue:
  ; D
  lda _2dx
  sec
  sbc _dy
  sta _D
  ; D_step_V
  lda _dx
  sec
  sbc _dy
  sta _D_step_V
  clc
  adc _D_step_V
  sta _D_step_V
  ; x
  lda pixel_start_x
  sta pixel_current_x
  
  rts

; #--------------------------------------------# ;

; general draw line:
;   - choose between vertical or horizontal line
;   - if necessary invert start and end pos
draw_line:
  jsr init_line

  ; if abs(dy) < abs(dx)
  lda _abs_dy
  cmp _abs_dx
  ; if true: use horizontal line
  bcc _horizontal
  ; if false: use vertical line
  jmp _vertical

  _horizontal:
    ; if start_x > end_x:
    lda pixel_start_x
    cmp pixel_end_x
    ; if true: swap start and end pos
    bcs _horizontal_swap
    jsr draw_line_horizontal
    jmp _end
  _horizontal_swap:
    jsr swap_start_end_pos
    ; dx and dy changes since pos swap, so need to be recalculated
    jsr init_line
    jsr draw_line_horizontal
    jmp _end
  _vertical:
    ; if start_y > end_y:
    lda pixel_start_y
    cmp pixel_end_y
    ; if true: swap start and end pos
    bcs _vertical_swap
    jsr draw_line_vertical
    jmp _end
  _vertical_swap:
    jsr swap_start_end_pos
    ; dx and dy changes since pos swap, so need to be recalculated
    jsr init_line
    jsr draw_line_vertical
    jmp _end

  _end:
    rts

; #--------------------------------------------# ;

; horizontal line algorithm, see ../resources/line_horizontal.png
draw_line_horizontal:
  jsr init_line_horizontal

  lda pixel_start_x
  sta pixel_current_x

  line_horizontal_loop:
    jsr pos_to_addr
    jsr plot_pixel

    lda _D
    beq line_horizontal_loop_continue
    bmi line_horizontal_loop_continue

    lda pixel_current_y
    clc
    adc _dir_y
    sta pixel_current_y
    lda _D
    clc
    adc _D_step_H
    sta _D
    jmp line_horizontal_loop_end_check
    
  line_horizontal_loop_continue:
    lda _D
    clc
    adc _2dy
    sta _D

  line_horizontal_loop_end_check:
    lda pixel_current_x
    inc pixel_current_x
    cmp pixel_end_x
    bne line_horizontal_loop

  rts

; #--------------------------------------------# ;

; horizontal line algorithm, see ../resources/line_vertical.png
draw_line_vertical:
  jsr init_line_vertical

  lda pixel_start_y
  sta pixel_current_y

  line_vertical_loop:
    jsr pos_to_addr
    jsr plot_pixel

    lda _D
    beq line_vertical_loop_continue
    bmi line_vertical_loop_continue

    lda pixel_current_x
    clc
    adc _dir_x
    sta pixel_current_x
    lda _D
    clc
    adc _D_step_V
    sta _D
    jmp line_vertical_loop_end_check
    
  line_vertical_loop_continue:
    lda _D
    clc
    adc _2dx
    sta _D

  line_vertical_loop_end_check:
    lda pixel_current_y
    inc pixel_current_y
    cmp pixel_end_y
    bne line_vertical_loop

  rts

; #--------------------------------------------# ;

; convert current pixel x,y to screen addr
; pixel_current_addr = $200 + y * 32 + x
pos_to_addr:
  ; screens starts at $200
  lda #$00
  sta pixel_current_L
  lda #$02
  sta pixel_current_H

  ldy pixel_current_y

  ; add 32 y times
  pta_loop_y:
    cpy #$00
    beq pta_x

    dey

    lda pixel_current_L
    clc
    adc #$20  ; 32
    sta pixel_current_L

    bcc pta_loop_y

    lda pixel_current_H
    adc #$00
    sta pixel_current_H

    jmp pta_loop_y

  ; add x
  pta_x:
    lda pixel_current_L
    clc
    adc pixel_current_x
    sta pixel_current_L

  rts

; #--------------------------------------------# ;

; plot a pixel of color color_drawing at the
; position saved in memory in pixelAddrL
plot_pixel:
  lda color_drawing
  sta (pixel_current_L, X)

  rts

; #--------------------------------------------# ;

swap_start_end_pos:
  ; start to tmp
  lda pixel_start_x
  sta pixel_swap_x
  lda pixel_start_y
  sta pixel_swap_y
  ; end to start
  lda pixel_end_x
  sta pixel_start_x
  lda pixel_end_y
  sta pixel_start_y
  ; tmp to end
  lda pixel_swap_x
  sta pixel_end_x
  lda pixel_swap_y
  sta pixel_end_y

  rts

; #--------------------------------------------# ;

; copy cursor x,y to current pixel x,y
copy_cursor_to_current:
  lda pixel_cursor_x
  sta pixel_current_x
  lda pixel_cursor_y
  sta pixel_current_y

  rts

; #--------------------------------------------# ;

; reset screen to all black
; go first to all white for small animation
reset_screen:
  lda #$01
  reset_screen_loop_white:
    sta $200, X
    sta $300, X
    sta $400, X
    sta $500, X
    inx
    bne reset_screen_loop_white

  lda #$00
  reset_screen_loop_black:
    sta $200, X
    sta $300, X
    sta $400, X
    sta $500, X
    inx
    bne reset_screen_loop_black

  rts

; #--------------------------------------------# ;

; negate number in accumulator
negate:
  eor #$ff
  clc
  adc #$01

  rts

; #--------------------------------------------# ;

delay:
  ldy #$88
  delay_loop:
    dey
    bne delay_loop

  rts

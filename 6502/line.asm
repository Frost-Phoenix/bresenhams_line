; Bresenham's line drawing algorithm
; https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#All_cases

; #--------------------------------------------# ;

define  pixel_current_L $00
define  pixel_current_H $01
define  pixel_current_x $02
define  pixel_current_y $03
define  pixel_start_x   $04
define  pixel_start_y   $05
define  pixel_end_x     $06
define  pixel_end_y     $07
define  pixel_swap_x    $08
define  pixel_swap_y    $09

define  _dx     $10
define  _dy     $11
define  _2dx    $12
define  _2dy    $13
define  _dir_x  $14
define  _dir_y  $15
define  _abs_dx $16
define  _abs_dy $17

define  _D        $20
define  _D_step_H $21 ; 2 * (dy - dx)
define  _D_step_V $22 ; 2 * (dx - dy)

; #--------------------------------------------# ;

main:
  lda #$00
  sta pixel_start_x
  lda #$1f
  sta pixel_start_y
  lda #$1a
  sta pixel_end_x
  lda #$00
  sta pixel_end_y

  jsr draw_line
  
  brk

; #--------------------------------------------# ;

init:
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

init_horizontal:
  ; dir_y
  lda #$01
  sta _dir_y
  lda _dy
  bpl init_horizontal_continue
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
init_horizontal_continue:
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

init_vertical:
  ; dir_x
  lda #$01
  sta _dir_x
  lda _dx
  bpl init_vertical_continue
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
init_vertical_continue:
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

draw_line:
  jsr init

  ; if abs(dy) < abs(dx)
  lda _abs_dy
  cmp _abs_dx
  ; if true:
  bcc _horizontal
  ; if false:
  jmp _vertical

  _horizontal:
    ; if start_x > end_x:
    lda pixel_start_x
    cmp pixel_end_x
    bcs _horizontal_swap
    jsr draw_line_horizontal
    jmp _end
  _horizontal_swap:
    jsr swap_start_end_pos
    ; dx and dy changes since pos swap, so neet to be recalculated
    jsr init
    jsr draw_line_horizontal
    jmp _end
  _vertical:
    ; if start_y > end_y:
    lda pixel_start_y
    cmp pixel_end_y
    bcs _vertical_swap
    jsr draw_line_vertical
    jmp _end
  _vertical_swap:
    jsr swap_start_end_pos
    ; dx and dy changes since pos swap, so neet to be recalculated
    jsr init
    jsr draw_line_vertical
    jmp _end

  _end:
    rts

; #--------------------------------------------# ;

draw_line_horizontal:
  jsr init_horizontal

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

draw_line_vertical:
  jsr init_vertical

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

; convert current pixel x,y to addr
; pixel_current_addr = $200 + y * 32 + x
pos_to_addr:
  ; screens starts at $200
  lda #$00
  sta pixel_current_L
  lda #$02
  sta pixel_current_H

  ldy pixel_current_y

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

  pta_x:
    lda pixel_current_L
    clc
    adc pixel_current_x
    sta pixel_current_L

  rts

; #--------------------------------------------# ;

; plot white pixel at the position saved in memory in pixelAddrL
plot_pixel:
  ; save X and A registers
  pha
  txa
  pha
  ldx #$00

  ; plot pixel
  lda #$01
  sta (pixel_current_L, X)

  ; restore X and A registers
  pla
  tax
  pla

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

negate:
  eor #$ff
  clc
  adc #$01
  rts

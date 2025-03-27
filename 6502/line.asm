; Bresenham's line drawing algorithm
; https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#All_cases

define  pixel_current_L $00
define  pixel_current_H $01
define  pixel_current_x $02
define  pixel_current_y $03
define  pixel_start_x   $04
define  pixel_start_y   $05
define  pixel_end_x     $06
define  pixel_end_y     $07

define  _dx     $10
define  _dy     $11
define  _2dx    $12
define  _2dy    $13
define  _dir_x  $14
define  _dir_y  $15

define  _D        $20
define  _D_step_H $21 ; 2 * (dy - dx)
define  _D_step_V $22 ; 2 * (dx - dy)

main:
  lda #$00
  sta pixel_start_x
  lda #$00
  sta pixel_start_y
  lda #$1f
  sta pixel_end_x
  lda #$09
  sta pixel_end_y

  jsr draw_line_horizontal
  
  brk

init_horizontal:
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
  ; dir_y
  lda #$01
  sta _dir_y
  lda _dy
  bpl init_horizontal_continue
  lda #$ff
  sta _dir_y
  lda _dy
  jsr negate
  sta _dy
init_horizontal_continue:
  ; 2dy
  clc
  adc _dy
  sta _2dy
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
  ; D_step_V
  ; y 
  lda pixel_start_y
  sta pixel_current_y
  
  rts

draw_line_horizontal:
  jsr init_horizontal

  lda pixel_start_x
  sta pixel_current_x

  line_horizontal_loop:
    jsr pos_to_addr
    jsr plot_pixel

    lda _D
    cmp #$00
    beq line_horizontal_loop_continue
    cmp #$00
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

negate:
  eor #$ff
  clc
  adc #$01
  rts

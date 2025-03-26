; Bresenham's line drawing algorithm

define  pixel_current_L $00
define  pixel_current_H $01
define  pixel_current_x $02
define  pixel_current_y $03
define  pixel_start_x   $04
define  pixel_start_y   $05
define  pixel_end_x     $06
define  pixel_end_y     $07

define  _dx   $10
define  _dy   $11
define  _2dx  $12
define  _2dy  $13
define  _D    $20

main:
  lda #$00
  sta pixel_start_x
  lda #$00
  sta pixel_start_y
  lda #$20
  sta pixel_end_x
  lda #$20
  sta pixel_end_y

  jsr draw_line

  ; jsr pos_to_addr
  ; jsr plot_pixel

  brk

init:
  ; dx
  sec
  lda pixel_end_x
  sbc pixel_start_x
  sta _dx
  ; 2dx
  clc
  adc _dx
  sta _2dx
  ; dy
  sec
  lda pixel_end_y
  sbc pixel_start_y
  sta _dy
  ; 2dy
  clc
  adc _dy
  sta _2dy
  ; D
  sec
  lda _2dy
  sbc _dx
  sta _D
  ; y 
  lda pixel_start_y
  sta pixel_current_y
  
  rts

draw_line:
  jsr init

  lda pixel_start_x
  sta pixel_current_x

  line_loop:
    jsr pos_to_addr
    jsr plot_pixel

    lda _D
    cmp #$00
    bmi line_loop_continue

    inc pixel_current_y
    sec
    lda _D
    sbc _2dx
    sta _D
    
  line_loop_continue:
    clc
    lda _D
    adc _2dy
    sta _D
    
    inc pixel_current_x
    lda pixel_current_x
    cmp pixel_end_x
    bne line_loop

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

    clc
    lda pixel_current_L
    adc #$20  ; 32
    sta pixel_current_L

    bcc pta_loop_y

    lda pixel_current_H
    adc #$00
    sta pixel_current_H

    jmp pta_loop_y

  pta_x:
    clc
    lda pixel_current_L
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

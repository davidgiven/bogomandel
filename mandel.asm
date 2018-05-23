oswrch = $ffee
romsel = $fe30
romsel_ram = $f4

ITERATIONS = 16
BORDER = 38

.zero
screenptr_top: .word 0
screenptr_bot: .word 0 ; mirrored pixel on the bottom of the display
screenx:    .byte 0
screeny:    .byte 0

pixelcol:   .byte 0
pixelmask:  .byte 0
temp:       .byte 0

boxx1:      .byte 0
boxy1:      .byte 0
boxx2:      .byte 0 ; INCLUSIVE (so 0..255)
boxy2:      .byte 0 ; INCLUSIVE (so 0..255)
midx:       .byte 0
midy:       .byte 0
sidecount:  .byte 0

corecolour: .byte 0
colourflag: .byte 0

ci:         .word 0
cr:         .word 0
zi:         .word 0
zr:         .word 0
zi2:        .word 0
zr2:        .word 0
zr_p_zi:    .word 0
zr2_p_zi2:  .word 0
zr2_m_zi2:  .word 0
iterations: .byte 0

#define PUSH(var) lda var : pha
#define POP(var) pla : sta var

.text
    * = $0e00
    jsr init_screen

    /* Map sideways RAM bank 4, containing our lookup table. */

    lda #4
    sei
    sta romsel
    sta romsel_ram
    cli

    ; Draw left.

    lda #0
    sta boxx1
    sta boxy1
    lda #127
    sta boxx2
    lda #127
    sta boxy2
    jsr box

    ; Draw right.

    lda #128
    sta boxx1
    lda #0
    sta boxy1
    lda #255
    sta boxx2
    lda #127
    sta boxy2
    jsr box

    ; Put BASIC back in the ROM slot to avoid hilarity on exit.
safeexit:
    lda #12
    sei
    sta romsel
    sta romsel_ram
    cli
    rts

box:
.(
    ; The line drawing routines don't draw the last pixel, so do that
    ; specially. (We need to probe one pixel anyway so it's no bad
    ; thing.)
    lda boxx2
    sta screenx
    lda boxy2
    sta screeny
    jsr calculate_screen_address
    jsr calculate

    lda pixelcol
    sta corecolour
    stz colourflag

    ; Top stroke

    lda boxx1
    sta screenx
    lda boxy1
    sta screeny
    lda boxx2
    sec
    sbc boxx1
    lsr
    sta sidecount
    jsr calculate_screen_address
    jsr hline

    ; Right stroke

    ; screenx, screeny preserved
    lda boxy2
    sec
    sbc boxy1
    sta sidecount
    jsr calculate_screen_address
    jsr vline

    ; Bottom stroke

    lda boxy2
    sta screeny
    lda boxx1
    sta screenx
    lda boxx2
    sec
    sbc boxx1
    lsr
    sta sidecount
    jsr calculate_screen_address
    jsr hline

    ; Left stroke

    lda boxx1
    sta screenx
    lda boxy1
    sta screeny
    lda boxy2
    sec
    sbc boxy1
    sta sidecount
    jsr calculate_screen_address
    jsr vline

    ; Are all the sides the same colour? If so, don't bother recursing.

    lda colourflag
    bne recurse
    jmp floodfill
recurse:

    ; Start recursion.

    PUSH(midx)
    PUSH(midy)

    ; Calculate centre point.

    lda boxx1
    lsr
    sta temp
    lda boxx2
    lsr
    clc
    adc temp
    and #$fe ; round down
    sta midx
    cmp boxx1
    beq too_small
    
    lda boxy1
    lsr
    sta temp
    lda boxy2
    lsr
    clc
    adc temp
    sta midy
    cmp boxy1
    beq too_small

    ; Recurse into top left.

    PUSH(boxx2)
    PUSH(boxy2)

    lda midx
    sta boxx2
    lda midy
    sta boxy2
    jsr box

    POP(boxy2)
    POP(boxx2)

    ; Recurse into bottom right.

    PUSH(boxx1)
    PUSH(boxy1)

    lda midx
    sta boxx1
    lda midy
    sta boxy1
    jsr box

    POP(boxy1)
    POP(boxx1)

    ; Recurse into bottom left.

    PUSH(boxx2)
    PUSH(boxy1)
    
    lda midx
    sta boxx2
    lda midy
    sta boxy1
    jsr box

    POP(boxy1)
    POP(boxx2)

    ; Recurse into top right.

    PUSH(boxx1)
    PUSH(boxy2)

    lda midx
    sta boxx1
    lda midy
    sta boxy2
    jsr box

    POP(boxy2)
    POP(boxx1)

too_small:
    POP(midy)
    POP(midx)

    rts

calculate:
.(
    jsr pick
    lda pixelcol
    cmp #$80
    bne dont_calculate

    jsr mandel
    jsr plot

dont_calculate:
    lda pixelcol
    sec
    sbc corecolour
    ora colourflag
    sta colourflag

    rts
.)

; Fill the current box with pixelcol.
floodfill:
.(
    lda boxy1
    inc
    cmp boxy2
    beq exit
    sta screeny
yloop:
    lda boxx1
    inc
    inc
    sta screenx
    lda boxx2
    sec
    sbc boxx1
    bcc exit
    lsr
    beq exit
    dec
    beq exit
    bmi exit
    sta sidecount
    jsr calculate_screen_address
xloop:
    lda corecolour
    sta pixelcol
    jsr plot
    jsr go_to_pixel_right
    dec sidecount
    bne xloop

    lda screeny
    inc
    sta screeny
    cmp boxy2
    bne yloop
exit:
    rts
.)

hline:
    jsr calculate
    jsr go_to_pixel_right
    dec sidecount
    bne hline
    rts

vline:
    jsr calculate
    jsr go_to_pixel_down
    dec sidecount
    bne vline
    rts
.)

; Loads screenptr_top with the address of the pixel at screenx/screeny.
calculate_screen_address:
.(
    lda screeny
    and #$f8            ; mask into char rows (0, 8, 16...)
    lsr
    lsr                 ; twice the row number (0, 2, 4...)
    tax

    ; Top half.

    lda row_table+0, x
    sta screenptr_top+0
    lda row_table+1, x
    adc #0
    sta screenptr_top+1

    ; Flip for the bottom half.

FLIPADDR = 7 + $3000 + $8000 - 640
    lda #<FLIPADDR
    sec
    sbc screenptr_top+0
    sta screenptr_bot+0
    lda #>FLIPADDR
    sbc screenptr_top+1
    sta screenptr_bot+1

    ; Calculate the X offset (into pixelmask/A)
    lda screenx     ; remember these are logical pixels (0..255)
    and #$fc        ; mask to char column
    stz pixelmask   ; reuse this as high byte of X offset
    asl             ; *2
    rol pixelmask
    
    ; Add on the X offset.
    ; C is already clear
    tax
    adc screenptr_top+0
    sta screenptr_top+0
    lda screenptr_top+1
    adc pixelmask
    sta screenptr_top+1
    txa

    clc
    adc screenptr_bot+0
    sta screenptr_bot+0
    lda screenptr_bot+1
    adc pixelmask
    sta screenptr_bot+1

    /* Now adjust for the interchar row. */

    lda screeny
    and #$07            ; interchar row
    sta pixelmask

    clc
    adc screenptr_top+0
    sta screenptr_top+0
    bcc dont_add_screenptr_top
    inc screenptr_top+1
dont_add_screenptr_top:

    sec
    lda screenptr_bot+0
    sbc pixelmask
    sta screenptr_bot+0
    bcs dont_add_screenptr_bot
    dec screenptr_bot+1
dont_add_screenptr_bot:

exit:
    rts

#define row_addr(y) .word ($3000 + y*640)
row_table:
    row_addr(0)
    row_addr(1)
    row_addr(2)
    row_addr(3)
    row_addr(4)
    row_addr(5)
    row_addr(6)
    row_addr(7)
    row_addr(8)
    row_addr(9)
    row_addr(10)
    row_addr(11)
    row_addr(12)
    row_addr(13)
    row_addr(14)
    row_addr(15)
    row_addr(16)
    row_addr(17)
    row_addr(18)
    row_addr(19)
    row_addr(20)
    row_addr(21)
    row_addr(22)
    row_addr(23)
    row_addr(24)
    row_addr(25)
    row_addr(26)
    row_addr(27)
    row_addr(28)
    row_addr(29)
    row_addr(30)
    row_addr(31)
.)

; Given a calculated screenptr_top, moves to the next horizontal physical pixel
; (which is two logical pixels because MODE 2).
go_to_pixel_right:
.(
    lda screenx
    inc
    inc
    sta screenx
    and #3
    bne exit

    clc
    lda screenptr_bot+0
    adc #8
    sta screenptr_bot+0
    bcc dont_add_screenptr_bot
    inc screenptr_bot+1
dont_add_screenptr_bot:

    clc
    lda screenptr_top+0
    adc #8
    sta screenptr_top+0
    bcc dont_add_screenptr_top
    inc screenptr_top+1
dont_add_screenptr_top:

exit:
    rts
.)

; Given a calculated screenptr_top, moves to the next vertical pixel.
go_to_pixel_down:
.(
    inc screenptr_top+0
    bne dont_increment_screenptr_top
    inc screenptr_top+1
dont_increment_screenptr_top:

    lda screenptr_bot+0
    dec
    sta screenptr_bot+0
    cmp #$ff
    bne dont_decrement_screenptr_bot
    dec screenptr_bot+1
dont_decrement_screenptr_bot:

    lda screeny
    inc
    sta screeny
    and #7
    bne exit

ROWSIZE = 640 - 8
    clc
    lda screenptr_top+0
    adc #<ROWSIZE
    sta screenptr_top+0
    lda screenptr_top+1
    adc #>ROWSIZE
    sta screenptr_top+1

    sec
    lda screenptr_bot+0
    sbc #<ROWSIZE
    sta screenptr_bot+0
    lda screenptr_bot+1
    sbc #>ROWSIZE
    sta screenptr_bot+1

exit:
    rts
.)

; Plot colour pixelcol to the pixel at screenx/screeny (calculate_screen_address must
; have been called). Corrupts pixelcol!
plot:
.(
    lda #$55
    sta pixelmask

    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    lda screenx     ; Is this an even pixel?
    ror
    ror             ; odd/even bit to C
    bcc even_pixel

    lsr pixelcol
    asl pixelmask

even_pixel:
    lda (screenptr_top)
    and pixelmask
    ora pixelcol
    sta (screenptr_top)
    sta (screenptr_bot)
    rts
.)

; Pick colour from screenx/screeny (calculate_screen_address must have been
; called) into pixelcol.
pick:
.(
    lda screenx
    ror
    ror             ; odd/even bit to C
    lda (screenptr_top)
    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    bcc even_pixel
    asl
even_pixel:
    and #$aa
    sta pixelcol
    rts
.)

palette:
    .byte %00000000
    .byte %00000010
    .byte %00001000
    .byte %00001010
    .byte %00100000
    .byte %00100010
    .byte %00101000
    .byte %00101010
    .byte %10000000
    .byte %10000010
    .byte %10001000
    .byte %10001010
    .byte %10100000
    .byte %10100010
    .byte %10101000
    .byte %10101010

init_screen:
    ldx #0
.(
loop:
    lda setup_bytes, x
    jsr oswrch
    inx
    cpx #(setup_bytes_end - setup_bytes)
    bne loop
    rts

setup_bytes:
    .byte 22, 2 ; mode 2
    .byte 19, 8, 0, 0, 0, 0 ; colour 0 = black
    .byte 28, 0, 31, 15, 0 ; text window
    .byte 17, 128+8 ; set background colour
    .byte 12 ; clear window
    .byte 28, 14, 31, 19, 0 ; text window
    .byte 17, 128+0 ; set background colour
    .byte 12 ; clear window
setup_bytes_end:
.)

/* Actually do the work of calculating the colour of a pixel (in screenx, screeny). */
mandel:
.(
    /* Turns screenx/screeny (0..255, midpoint 0x80) into ci/cr (-2..2). */

sei
    stz cr+1
    lda screenx
    clc
    adc #$80            /* adjust midpoint */
    bpl x_not_negative
    dec cr+1            /* if negative, sign extend high byte */
x_not_negative:
    asl                 /* the number in cr+1:A is now -0.5..0.5, so double */
    sta cr+0
    lda cr+1
    rol
    asl cr+0            /* and again */
    rol
    asl cr+0            /* and again */
    rol
    and #$3f            /* fixup the high byte to be an address */
    ora #$80
    sta cr+1
    sta zr+1
    lda cr+0
    sta zr+0

    stz ci+1
    lda screeny
    clc
    adc #$80            /* adjust midpoint */
    bpl y_not_negative
    dec ci+1            /* if negative, sign extend high byte */
y_not_negative:
    asl                 /* the number in ci+1:A is now -1..1, so double */
    sta ci+0
    lda ci+1
    rol
    asl ci+0            /* and again */
    rol
    asl ci+0            /* and again */
    rol
    and #$3f            /* fixup the high byte to be an address */
    ora #$80
    sta ci+1
    sta zi+1
    lda ci+0
    sta zi+0

    /* Now we go into reenigne's Mandelbrot kernel. */

    lda #ITERATIONS
    sta iterations
    ldy #1              /* indexing with this accesses the high byte */
loop:
    /* Calculate zr^2 + zi^2. */

    clc
    lda (zr)            /* A = low(zr^2) */
    tax                 
    adc (zi)            /* A = low(zr^2) + low(zi^2) = low(zr^2 + zi^2) */
    sta zr2_p_zi2+0
    lda (zr), y         /* A = high(zr^2) */
    adc (zi), y         /* A = high(zr^2) + high(zi^2) = high(zr^2 + zi^2) */
    sta zr2_p_zi2+1

    /* Test for bailout: (zr^2 + zi^2) < 4. */

    asl
    asl                 /* put sign bit at top */
    sec
    sbc #$20            /* test bailout: $2000>>2 = $0800 = 4.0 */
    bvc no_signed_adjustment
    eor #$80
no_signed_adjustment:
    bpl bailout
    
    /* Calculate zr + zi. */

    clc
    lda zr+0            /* A = low(zr) */
    adc zi+0            /* A = low(zr + zi) */
    sta zr_p_zi+0
    lda zr+1            /* A = high(zr) */
    adc zi+1            /* A = high(zr + zi) + C */
    and #$3f
    ora #$80            /* fixup */
    sta zr_p_zi+1

    /* Calculate zr^2 - zi^2. */

    txa                 /* A = low(zr^2) */
    sec
    sbc (zi)            /* A = low(zr^2 - zi^2) */
    sta zr2_m_zi2+0
    lda (zr), y         /* A = high(zr^2) */
    sbc (zi), y         /* A = high(zr^2 - zi^2) */
    sta zr2_m_zi2+1

    /* Calculate zr = (zr^2 - zi^2) + cr. */

    clc
    lda zr2_m_zi2+0     /* A = low(zr^2 - zi^2) */
    adc cr+0            /* A = low(zr^2 - zi^2 + cr) */
    sta zr+0
    lda zr2_m_zi2+1     /* A = high(zr^2 - zi^2) */
    adc cr+1            /* A = high(zr^2 - zi^2 + cr) */
    and #$3f
    ora #$80            /* fixup */
    sta zr+1

    /* Calculate zi' = (zr+zi)^2 - (zr^2 + zi^2). */

    sec
    lda (zr_p_zi)       /* A = low((zr + zi)^2) */
    sbc zr2_p_zi2+0     /* A = low((zr + zi)^2 - (zr^2 + zi^2)) */
    sta zi+0            /* not really, temp storage */
    lda (zr_p_zi), y    /* A = high((zr + zi)^2) */
    sbc zr2_p_zi2+1     /* A = high((zr + zi)^2 - (zr^2 + zi^2)) */
    sta zi+1            /* not really, temp storage */

    /* Calculate zi = zi' + ci. */

    clc
    lda zi+0
    adc ci+0
    sta zi+0
    lda zi+1
    adc ci+1
    and #$3f
    ora #$80            /* fixup */
    sta zi+1

    dec iterations
    bne loop

bailout:
    lda iterations
    and #7
    tax
    lda palette, x
    sta pixelcol
cli
    rts
.)

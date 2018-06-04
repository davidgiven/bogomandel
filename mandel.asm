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

mulptr:     .word 0

ci:         .byte 0
cr:         .byte 0
zi:         .byte 0
zr:         .byte 0
zi2:        .byte 0
zr2:        .byte 0
iterations: .byte 0

#define PUSH(var) lda var : pha
#define POP(var) pla : sta var

.text
    * = $0e00
    jsr init_screen
    jsr setup_unsigned_tables
    jsr setup_signed_tables

    ; Draw left.

    lda #BORDER
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
    lda #BORDER
    sta boxy1
    lda #255-BORDER
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

lookup:
    jsr map_multiplication
    lda (mulptr)
    sta $70
    bra safeexit

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
    lda pixelcol
    pha
    jsr plot
    pla

dont_calculate:
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

    inc screeny
    lda screeny
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
    bcs add_screenptr_top
    sec
after_add_screenptr_top:

    lda screenptr_bot+0
    sbc pixelmask
    sta screenptr_bot+0
    bcc add_screenptr_bot

exit:
    rts

add_screenptr_top:
    inc screenptr_top+1
    bra after_add_screenptr_top

add_screenptr_bot:
    dec screenptr_bot+1
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
    bcs add_screenptr_bot
after_add_screenptr_bot:

    lda screenptr_top+0
    adc #8
    sta screenptr_top+0
    bcs add_screenptr_top

exit:
    rts

add_screenptr_bot:
    inc screenptr_bot+1
    clc
    bra after_add_screenptr_bot

add_screenptr_top:
    inc screenptr_top+1
    rts

.)

; Given a calculated screenptr_top, moves to the next vertical pixel.
go_to_pixel_down:
.(
    inc screenptr_top+0
    beq increment_screenptr_top
after_increment_screenptr_top:

    lda screenptr_bot+0
    beq decrement_screenptr_bot
after_decrement_screenptr_bot:
    dec screenptr_bot+0

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

increment_screenptr_top:
    inc screenptr_top+1
    bra after_increment_screenptr_top

decrement_screenptr_bot
    dec screenptr_bot+1
    bra after_decrement_screenptr_bot
.)

; Plot colour pixelcol to the pixel at screenx/screeny (calculate_screen_address must
; have been called). Corrupts pixelcol!
plot:
.(
    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    lda screenx     ; Is this an even pixel?
    ror
    ror             ; odd/even bit to C
    lda #$55
    bcc even_pixel

    lsr pixelcol
    asl

even_pixel:
    and (screenptr_top)
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

; Points mulptr at the result of signed X*Y>>8.
; Preserves X, Y.
map_multiplication:
    ; Set up RAM bank.
    tya
    rol
    rol
    rol
    and #%00000011
    clc
    adc #4 ; RAM banks are in 4, 5, 6, 7
    sei
    sta romsel
    sta romsel_ram
    cli

    tya
    and #%00111111
    ora #%10000000 ; add $80, the high byte of sideways RAM
    sta mulptr+1
    stx mulptr+0
    rts
    
; Sets up the unsigned multiplication tables in sideway RAM. Abuses screenptr_top
; for workspace.
setup_unsigned_tables:
.(
    ldy #0
yloop:
    ldx #0
    stz screenptr_top+0
    stz screenptr_top+1
xloop:
    jsr map_multiplication

    /* Load the multiplication result into A:temp. */
    lda screenptr_top+0
    sta temp
    lda screenptr_top+1

    /* Shift right by 6 for the fixed point adjustment. */
    /* (Implemented as shifting left by 2 and taking the high byte. */

    asl temp
    rol
    asl temp
    rol
    bmi overflow
    sta (mulptr)

    /* Add 8-bit Y to (screenptr_top). */
    tya
    clc
    adc screenptr_top+0
    sta screenptr_top+0
    lda screenptr_top+1
    adc #0
    sta screenptr_top+1
    bmi overflow

nextx:
    inx
    bpl xloop
nexty:
    iny
    bpl yloop
    rts

overflow:
    lda #$7f
    sta (mulptr)
    bra nextx
.)

/* Once the unsigned tables have been generated, we can use this to fill in
 * the gaps for negative numbers.
 */
setup_signed_tables:
.(
    ldy #0
yloop:
    ldx #0
xloop:
    txa
    eor #$ff
    inc
    sta screenx

    tya
    eor #$ff
    inc
    sta screeny

    jsr map_multiplication
    lda (mulptr)

    ; Positive result goes in negx, negy.

    phx
    ldx screenx
    phy
    ldy screeny
    pha
    jsr map_multiplication
    pla
    sta (mulptr)
    ply

    ; Everything else uses a negative result.

    eor #$ff
    inc

    ; negx, posy.

    ldx screenx
    pha
    jsr map_multiplication
    pla
    sta (mulptr)
    plx

    phy
    ldy screeny
    pha
    jsr map_multiplication
    pla
    sta (mulptr)
    ply

    inx
    bpl xloop
    iny
    bpl yloop
    rts
.)

/* Actually do the work of calculating the colour of a pixel (in screenx, screeny). */
mandel:
.(
    lda screeny
    eor #$80
    sta ci
    sta zi

    lda screenx
    eor #$80
    sta cr
    sta zr
    lda #ITERATIONS
    sta iterations

loop:
    ldx zr
    ldy zr
    jsr map_multiplication
    lda (mulptr)
    cmp #$7f
    beq bailout
    sta zr2

    ldx zi
    ldy zi
    jsr map_multiplication
    lda (mulptr)
    cmp #$7f
    beq bailout
    sta zi2

    ldx zr
    ldy zi
    jsr map_multiplication
    lda (mulptr)
    cmp #$7f
    beq bailout
    
    asl
    clc
    adc ci
    sta zi

    lda zr2
    sec
    sbc zi2
    clc
    adc cr
    sta zr

    dec iterations
    bne loop

bailout:
    lda iterations
    and #7
    tax
    lda palette, x
    sta pixelcol
    rts
.)

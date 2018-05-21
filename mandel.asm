oswrch = $ffee

.zero
screenptr:  .word 0
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

xc:         .byte 0

#define PUSH(var) lda var : pha
#define POP(var) pla : sta var

.text
    * = $0e00
    jsr setup
    lda #0
    sta boxx1
    sta boxy1
    lda #255
    sta boxx2
    sta boxy2
    jsr box
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
    beq dont_recurse
    
    lda boxy1
    lsr
    sta temp
    lda boxy2
    lsr
    clc
    adc temp
    sta midy
    cmp boxy1
    beq dont_recurse

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

dont_recurse:
    POP(midy)
    POP(midx)

    rts

calculate:
    lda palette+7
    sta pixelcol
    jsr plot
    rts

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

; Loads screenptr with the address of the pixel at screenx/screeny.
calculate_screen_address:
.(
    lda screeny
    and #$f8            ; mask into char rows (0, 8, 16...)
    lsr
    lsr                 ; twice the row number (0, 2, 4...)
    tax

    lda screeny
    and #$07            ; inter-char row
    clc
    adc row_table+0, x
    sta screenptr+0
    lda row_table+1, x
    adc #0
    sta screenptr+1

    ; Calculate the X offset (into pixelmask/A)
    lda screenx     ; remember these are logical pixels (0..255)
    and #$fc        ; mask to char column
    stz pixelmask   ; reuse this as high byte of X offset
    asl             ; *2
    rol pixelmask
    
    ; Add on the X offset.
    ; C is already clear
    adc screenptr+0
    sta screenptr+0
    lda screenptr+1
    adc pixelmask
    sta screenptr+1

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

; Given a calculated screenptr, moves to the next horizontal physical pixel
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
    lda screenptr+0
    adc #8
    sta screenptr+0
    bcc exit
    inc screenptr+1

exit:
    rts
.)

; Given a calculated screenptr, moves to the next vertical pixel.
go_to_pixel_down:
.(
    inc screenptr+0
    bne dont_increment_screenptr
    inc screenptr+1
dont_increment_screenptr:

    lda screeny
    inc
    sta screeny
    and #7
    bne exit

    clc
    lda screenptr+0
    adc #$78
    sta screenptr+0
    lda screenptr+1
    adc #$02
    sta screenptr+1
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
    lda (screenptr)
    and pixelmask
    ora pixelcol
    sta (screenptr)
    rts
.)

; Pick colour from screenx/screeny (calculate_screen_address must have been
; called) into pixelcol.
pick:
.(
    lda screenx
    ror
    ror             ; odd/even bit to C
    lda (screenptr)
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

setup:
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
    .byte 23, 1, 0, 0, 0, 0, 0, 0, 0, 0 ; cursor off
    .byte 19, 8, 4, 0, 0, 0 ; colour 4 = blue
    .byte 28, 0, 31, 15, 0 ; text window
    .byte 17, 128+8 ; set background colour
    .byte 12 ; clear window
    .byte 28, 16, 31, 19, 0 ; text window
    .byte 17, 128+0 ; set background colour
    .byte 12 ; clear window
setup_bytes_end:
.)

oswrch = $ffee

.zero
screenptr:  .word 0
screenx:    .byte 0
screeny:    .byte 0

pixelcol:   .byte 0
pixelmask:  .byte 0

xc:         .byte 0

.text
    * = $0e00
    jsr setup

.(
    stz xc
loop:
    lda xc
    sta screeny
    lsr
    sta screenx
    jsr calculate_screen_address
    lda palette+7
    sta pixelcol
    jsr plot

    inc xc
    bne loop
.)

    rts

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

    ; Add the X offset
    lda screenx
    and #$fe        ; mask off even/odd pixels; range is 0..159
    stz pixelmask   ; reuse this as high byte of X offset
    asl             ; *2
    rol pixelmask
    asl             ; *4
    rol pixelmask
    
    clc
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

; Plot colour pixelcol to the pixel at screenx/screeny (calculate_screen_address must
; have been called). Corrupts pixelcol!
plot:
.(
    lda #$55
    sta pixelmask

    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    lda screenx     ; Is this an even pixel?
    and #1
    beq even_pixel

    lsr pixelcol
    asl pixelmask

even_pixel:
    lda (screenptr)
    and pixelmask
    ora pixelcol
    sta (screenptr)
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
    .byte 28, 16, 31, 19, 0 ; text window
setup_bytes_end:
.)

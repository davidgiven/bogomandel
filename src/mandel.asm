ITERATIONS = 32

fraction_bits = 9
integer_bits = 5
total_bits = fraction_bits + integer_bits

oswrch     = &FFEE
osbyte     = &FFF4
romsel     = &FE30
romsel_ram = &F4

mc_base = &2000
mc_top  = &3000

cpu 1 ; 65C02

puttext "src/!boot", "!boot", 0
putbasic "src/loader.bas", "loader"

; --- Global page ------------------------------------------------------------

org &70
.screenptr      equw 0
.screenx        equb 0
.screeny        equb 0

.pixelcol       equb 0
.pixelmask      equb 0
.temp           equb 0

.boxx1          equb 0
.boxy1          equb 0
.boxx2          equb 0 ; INCLUSIVE (so 0..255)
.boxy2          equb 0 ; INCLUSIVE (so 0..255)
.midx           equb 0
.midy           equb 0
.sidecount      equb 0

.corecolour     equb 0
.colourflag     equb 0

.centerx        equw 0
.centery        equw 0
.step           equb 0

.cr             equw 0
.ci             equw 0
.zi             equw 0
.zr             equw 0
.zr_p_zi        equw 0
.iterations     equb 0

; --- Main program ----------------------------------------------------------

clear mc_base, mc_top
org mc_base
guard mc_top

    ; ...and this gets invoked once the program's successfully copied.

    ; Initialisation.

.main_program_start
    jsr clear_screen
    jsr build_row_table
    jsr build_col_table

    ; Map sideways RAM bank 4, containing our lookup table.

    lda #4
    jsr map_rom

    ; Zoom settings.

    stz centerx+0
    stz centery+0
    stz centerx+1
    stz centery+1
    lda #8
    sta step
    jsr build_pixels_to_z_table

    ; Draw.

    lda #0
    sta boxx1
    sta boxy1
    lda #255
    sta boxx2
    lda #255
    sta boxy2
    jsr box

    ; Put BASIC back in the ROM slot to avoid hilarity on exit.

    lda #12
    jsr map_rom
    rts

; Maps the ROM in A.
.map_rom
    sei
    sta romsel
    sta romsel_ram
    cli
    rts


.box
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
    lsr A
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
    lsr A
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
.recurse

    ; Start recursion.

    lda midx: pha
    lda midy: pha

    ; Calculate centre point.

    lda boxx1
    lsr A
    sta temp
    lda boxx2
    lsr A
    clc
    adc temp
    and #&FE ; round down
    sta midx
    cmp boxx1
    beq box_too_small
    
    lda boxy1
    lsr A
    sta temp
    lda boxy2
    lsr A
    clc
    adc temp
    sta midy
    cmp boxy1
    beq box_too_small

    ; Recurse into top left.

    lda boxx2: pha
    lda boxy2: pha

    lda midx
    sta boxx2
    lda midy
    sta boxy2
    jsr box

    pla: sta boxy2
    pla: sta boxx2

    ; Recurse into bottom right.

    lda boxx1: pha
    lda boxy1: pha

    lda midx
    sta boxx1
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    pla: sta boxx1

    ; Recurse into bottom left.

    lda boxx2: pha
    lda boxy1: pha
    
    lda midx
    sta boxx2
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    pla: sta boxx2

    ; Recurse into top right.

    lda boxx1: pha
    lda boxy2: pha

    lda midx
    sta boxx1
    lda midy
    sta boxy2
    jsr box

    pla: sta boxy2
    pla: sta boxx1

.box_too_small
    pla: sta midy
    pla: sta midx

    rts


; Given a screenx/screeny and a calculated screen position, lazily renders the point.
.calculate
    jsr pick
    lda pixelcol
    cmp #&80
    bne dont_calculate

    ; Turns screenx/screeny (0..255, midpoint 0x80) into ci/cr (-2..2).

    ldx screenx
    lda pixels_to_zr_lo, X
    sta cr+0
    sta zr+0
    lda pixels_to_zr_hi, X
    sta cr+1
    sta zr+1

    ldy screeny
    lda pixels_to_zi_lo, Y
    sta ci_lo
    sta zi+0
    lda pixels_to_zi_hi, Y
    sta ci_hi
    sta zi+1

    jsr mandel
    lda pixelcol
    pha
    jsr plot
    pla

.dont_calculate:
    sec
    sbc corecolour
    ora colourflag
    sta colourflag

    rts


; Once zr, zi, cr, ci have been set up, use reenigne's Mandelbrot kernel to
; calculate the colour.
.mandel
    lda #ITERATIONS
    sta iterations
.iterator_loop
    ldy #1              ; indexing with this accesses the high byte 

    ; Calculate zr^2 + zi^2. 

    clc
    lda (zr)            ; A = low(zr^2) 
    tax                 
    adc (zi)            ; A = low(zr^2) + low(zi^2) = low(zr^2 + zi^2) 
    sta zr2_p_zi2_lo
    lda (zr), y         ; A = high(zr^2) 
    adc (zi), y         ; A = high(zr^2) + high(zi^2) = high(zr^2 + zi^2) 
    cmp #4 << (fraction_bits-8)
    bcs bailout
    sta zr2_p_zi2_hi

    ; Calculate zr + zi. 

    clc
    lda zr+0            ; A = low(zr) 
    adc zi+0            ; A = low(zr + zi) 
    sta zr_p_zi+0
    lda zr+1            ; A = high(zr) 
    adc zi+1            ; A = high(zr + zi) + C 
    and #&3F
    ora #&80            ; fixup 
    sta zr_p_zi+1

    ; Calculate zr^2 - zi^2. 

    txa                 ; A = low(zr^2) 
    sec
    sbc (zi)            ; A = low(zr^2 - zi^2) 
    tax
    lda (zr), y         ; A = high(zr^2) 
    sbc (zi), y         ; A = high(zr^2 - zi^2) 
    sta zr2_m_zi2_hi

    ; Calculate zr = (zr^2 - zi^2) + cr. 

    clc
    txa
    adc cr+0            ; A = low(zr^2 - zi^2 + cr) 
    sta zr+0
zr2_m_zi2_hi = *+1
    lda #99             ; A = high(zr^2 - zi^2) 
    adc cr+1            ; A = high(zr^2 - zi^2 + cr) 
    and #&3F
    ora #&80            ; fixup 
    sta zr+1

    ; Calculate zi' = (zr+zi)^2 - (zr^2 + zi^2). 

    sec
    lda (zr_p_zi)       ; A = low((zr + zi)^2) 
zr2_p_zi2_lo = *+1
    sbc #99             ; A = low((zr + zi)^2 - (zr^2 + zi^2)) 
    tax
    lda (zr_p_zi), y    ; A = high((zr + zi)^2) 
zr2_p_zi2_hi = *+1
    sbc #99             ; A = high((zr + zi)^2 - (zr^2 + zi^2)) 
    tay

    ; Calculate zi = zi' + ci. 

    clc
    txa
ci_lo = *+1
    adc #99
    sta zi+0
    tya
ci_hi = *+1
    adc #99
    and #&3F
    ora #&80            ; fixup 
    sta zi+1

    dec iterations
    bne iterator_loop

.bailout
    lda iterations
    and #7
    tax
    lda palette, X
    sta pixelcol
    rts


.hline
    jsr calculate
    jsr go_to_pixel_right
    dec sidecount
    bne hline
    rts

.vline
    jsr calculate
    jsr go_to_pixel_down
    dec sidecount
    bne vline
    rts


; Fill the current box with pixelcol.
.floodfill
    lda boxy1
    inc A
    cmp boxy2
    beq floodfill_exit
    sta screeny
.floodfill_yloop
    lda boxx1
    inc A
    inc A
    sta screenx
    lda boxx2
    sec
    sbc boxx1
    bcc floodfill_exit
    lsr A
    beq floodfill_exit
    dec A
    beq floodfill_exit
    bmi floodfill_exit
    sta sidecount
    jsr calculate_screen_address
.floodfill_xloop
    lda corecolour
    sta pixelcol
    jsr plot
    jsr go_to_pixel_right
    dec sidecount
    bne floodfill_xloop

    lda screeny
    inc A
    sta screeny
    cmp boxy2
    bne floodfill_yloop
.floodfill_exit
    rts


; Loads screenptr with the address of the pixel at screenx/screeny.
.calculate_screen_address
    lda screenx
    lsr A ; to physical pixels
    lsr A ; to pixels/2
    tax

    ldy screeny
    clc
    lda row_table_lo, Y
    adc col_table_lo, X
    sta screenptr+0
    lda row_table_hi, Y
    adc col_table_hi, X
    sta screenptr+1

    rts


; Given a calculated screenptr, moves to the next horizontal physical pixel
; (which is two logical pixels because MODE 2).
.go_to_pixel_right
    lda screenx
    inc A
    inc A
    sta screenx
    and #3
    bne go_to_pixel_right_exit

    clc
    lda screenptr+0
    adc #8
    sta screenptr+0
    bcc dont_add_screenptr
    inc screenptr+1
.dont_add_screenptr

.go_to_pixel_right_exit
    rts


; Given a calculated screenptr, moves to the next vertical pixel.
.go_to_pixel_down:
    inc screenptr+0
    bne dont_increment_screenptr
    inc screenptr+1
.dont_increment_screenptr

    lda screeny
    inc A
    sta screeny
    and #7
    bne go_to_pixel_down_exit

    clc
    lda screenptr+0
    adc #(640-8) MOD 256
    sta screenptr+0
    lda screenptr+1
    adc #(640-8) DIV 256
    sta screenptr+1

.go_to_pixel_down_exit
    rts


; Plot colour pixelcol to the pixel at screenx/screeny (calculate_screen_address must
; have been called). Corrupts pixelcol!
.plot:
    lda #&55
    sta pixelmask

    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    lda screenx     ; Is this an even pixel?
    ror A
    ror A           ; odd/even bit to C
    bcc plot_even_pixel

    lsr pixelcol
    asl pixelmask

.plot_even_pixel
    lda (screenptr)
    and pixelmask
    ora pixelcol
    sta (screenptr)
    rts


; Pick colour from screenx/screeny (calculate_screen_address must have been
; called) into pixelcol.
.pick
    lda screenx
    ror A
    ror A ; odd/even bit to C
    lda (screenptr)
    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    bcc pick_even_pixel
    asl A
.pick_even_pixel
    and #&AA
    sta pixelcol
    rts


; Clears the screen between renders.
.clear_screen
{
    ldx #0
.loop
    lda bytes, X
    jsr oswrch
    inx
    cpx #(bytes_end - bytes)
    bne loop
    rts

.bytes
    equb 28, 0, 31, 31, 0   ; define left-hand text window
    equb 17, 128+8          ; set background to marker colour
    equb 12                 ; clear window
    equb 28, 32, 31, 39, 0  ; define right-hand text window
    equb 17, 128+0          ; set background to black
    equb 12                 ; clear window
.bytes_end
}


; Build the pixels-to-z table.
.build_pixels_to_z_table
    ; Load cr and ci with step*128 (half a screen width).

    stz cr+0
    lda step ; A:(cr+0) = step * 256

    lsr A
    ror cr+0 ; A:(cr+0) = step * 128

    sta cr+1
    sta ci+1
    lda cr+0
    sta ci+0

    ; Now set zr and zi to the top and left of the image.

    sec
    lda centerx+0
    sbc cr+0
    sta zr+0
    lda centerx+1
    sbc cr+1
    sta zr+1

    sec
    lda centery+0
    sbc ci+0
    sta zi+0
    lda centery+1
    sbc ci+1
    sta zi+1

    ldx #0
.build_pixels_to_z_loop
    clc
    lda zr+0
    sta pixels_to_zr_lo, X
    adc step
    sta zr+0

    lda zr+1
    and #&3F
    ora #&80
    sta pixels_to_zr_hi, X
    adc #0
    sta zr+1

    clc
    lda zi+0
    sta pixels_to_zi_lo, X
    adc step
    sta zi+0

    lda zi+1
    and #&3F
    ora #&80
    sta pixels_to_zi_hi, X
    adc #0
    sta zi+1

    inx
    bne build_pixels_to_z_loop
    rts


; Build the column table (pixels/2 to address offset).
.build_col_table
    stz screenptr+0
    stz screenptr+1
    ldx #0
.build_col_table_loop
    clc
    lda screenptr+0
    sta col_table_lo, X
    adc #8
    sta screenptr+0

    lda screenptr+1
    sta col_table_hi, X
    adc #0
    sta screenptr+1

    inx
    bpl build_col_table_loop
    rts


; Build the row table (pixels to address).
.build_row_table
    stz screenptr+0
    lda #&30 ; framebuffer at &3000
    sta screenptr+1
    ldx #0
.build_row_table_loop
    clc
    lda screenptr+0
    sta row_table_lo, X
    adc #1
    sta screenptr+0

    lda screenptr+1
    sta row_table_hi, X
    adc #0
    sta screenptr+1

    inx
    beq build_row_table_loop_exit
    txa
    and #7
    bne build_row_table_loop

    ; Reached the end of a char row; increment by (640-8) to move
    ; to the next char row.

    clc
    lda screenptr+0
    adc #(640-8) MOD 256
    sta screenptr+0
    lda screenptr+1
    adc #(640-8) DIV 256
    sta screenptr+1

    bra build_row_table_loop
.build_row_table_loop_exit
    rts


; Maps logical colours (0..15) to MODE 2 left-hand-pixel values.
.palette
    equb &00
    equb &02
    equb &08
    equb &0A
    equb &20
    equb &22
    equb &28
    equb &2A
    equb &80
    equb &82
    equb &88
    equb &8A
    equb &A0
    equb &A2
    equb &A8
    equb &AA

.main_program_end

; Uninitialised data follows.

.pixels_to_zr_lo    skip &100
.pixels_to_zr_hi    skip &100
.pixels_to_zi_lo    skip &100
.pixels_to_zi_hi    skip &100
.col_table_lo       skip &80
.col_table_hi       skip &80
.row_table_lo       skip &100
.row_table_hi       skip &100

print "Program top:", ~P%
save "mandel", main_program_start, main_program_end

; --- Screen mode setup -----------------------------------------------------

clear mc_base, mc_top
org mc_base
guard mc_top

.setscreen_start
    lda #22: jsr oswrch
    lda #1: jsr oswrch    ; Note mode 1 (so text windows work)

    lda #39:  sta &30A    ; Characters per line
    lda #16:  sta &34F    ; Bytes per character
    lda #&0F: sta &360    ; Number of colours
    lda #&02: sta &361    ; Pixels per byte
    lda #&AA: sta &362    ; Pixel left mask
    lda #&55: sta &363    ; Pixel right mask
    lda #154: ldx #&F4: jsr osbyte ; Video ULA control register

    ldx #0
.init_screen_loop
    lda setup_bytes, X
    jsr oswrch
    inx
    cpx #(setup_bytes_end - setup_bytes)
    bne init_screen_loop

    ; Load jgh's special thin character set.

    lda #charset MOD 256
    sta screenptr+0
    lda #charset DIV 256
    sta screenptr+1

    ldx #32
.charloop
    lda #23
    jsr oswrch
    txa
    jsr oswrch

    ldy #0
.byteloop
    txa
    lsr A               ; odd/even bit to C
    lda (screenptr), Y
    bcc dont_shift_nibble
    asl A: asl A: asl A: asl A
.dont_shift_nibble
    and #&F0
    jsr oswrch
    
    iny
    cpy #8
    bne byteloop

    txa
    lsr A               ; odd/even bit to C
    bcc dont_advance

    clc
    tya
    adc screenptr+0
    sta screenptr+0
    lda screenptr+1
    adc #0
    sta screenptr+1

.dont_advance
    inx
    cpx #127
    bne charloop
    rts

.setup_bytes
    equb 20                            ; reset palette
    equb 23, 1, 0, 0, 0, 0, 0, 0, 0, 0 ; cursor off
    equb 19, 8, 0, 0, 0, 0             ; redefine special marker colour to black
.setup_bytes_end

.charset
    incbin "ThinSet"
.setscreen_end

save "setscrn", setscreen_start, setscreen_end

; --- Table of squares ------------------------------------------------------

org &8000
guard &c000

.squares_start
{
    for i, 0, (1<<total_bits)-1, 2
        if i >= &2000
            extended = i - &4000
        else
            extended = i
        endif
        equw ((extended^2) >> fraction_bits) and &3ffe or &8000
    next
}
.squares_end

save "squares", squares_start, squares_end

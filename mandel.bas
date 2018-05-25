ITERATIONS = 32

fixedmul = 2^9

HIMEM = &7000
program_buffer = &7000
buffer_size = &1000
program_start = 0
program_end = 0
program_size = 0

pixels_to_zr_lo = 0
pixels_to_zr_hi = 0
pixels_to_zi_lo = 0
pixels_to_zi_hi = 0
col_table_lo = 0
col_table_hi = 0
row_table_lo = 0
row_table_hi = 0

oswrch     = &FFEE
osbyte     = &FFF4
romsel     = &FE30
romsel_ram = &F4

run_location = &E00

PRINT "pass 1": PROCassemble(4)
PRINT "pass 2": PROCassemble(6)
PRINT "pass 3": PROCassemble(6)

PRINT "Loading charset to &"; ~charset_o
OSCLI "LOAD ThinSet " + STR$~charset_o

PRINT "Program top: &"; ~P%
PRINT "Total size:  &"; ~(O% - program_buffer)
PRINT "...main:     &"; ~(program_end - program_start)

OSCLI "SAVE mandelbin " + STR$~program_start + " " + STR$~program_end

*EXEC
INPUT '"Press RETURN to start"; A$
*EXEC !run
END

DEF PROCassemble(pass)
P% = &70
O% = program_buffer
[OPT pass
.screenptr  equw 0
.screenx        equb 0
.screeny        equb 0

.pixelcol       equb 0
.pixelmask      equb 0
.temp           equb 0

.boxx1          equb 0
.boxy1          equb 0
.boxx2          equb 0 \ INCLUSIVE (so 0..255)
.boxy2          equb 0 \ INCLUSIVE (so 0..255)
.midx           equb 0
.midy           equb 0
.sidecount      equb 0

.corecolour     equb 0
.colourflag     equb 0

.top            equw 0
.left           equw 0
.step           equb 0

.cr             equw 0
.ci             equw 0
.zi             equw 0
.zr             equw 0
.zr_p_zi        equw 0
]

P% = program_buffer
O% = program_buffer
[OPT pass
    \ This is the bootstrap code which copies the program from the assembly buffer
    \ to the run location.

    \ Now, copy the actual program to &E00.
    ldx #0
.copy_program_loop
]
FOR page = 0 TO (program_size + &FF) DIV 256
[OPT pass
    lda program_start + page*256, X
    sta run_location + page*256, X
]
NEXT
[OPT pass
    inx
    bne copy_program_loop

    \ And go.
    jmp run_location
]

P% = &E00
program_start = O%
[OPT pass
    \ ...and this gets invoked once the program's successfully copied.

    \ Initialisation.

    jsr set_charset
    jsr init_screen
    jsr clear_screen
    jsr build_row_table
    jsr build_col_table

    \ Map sideways RAM bank 4, containing our lookup table.

    lda #4
    jsr map_rom

    \ Zoom settings.

    lda #FNtofixed(-2) MOD 256
    sta top+0
    sta left+0
    lda #FNtofixed(-2) DIV 256
    sta top+1
    sta left+1
    lda #8
    sta step
    jsr build_pixels_to_z_table

    \ Draw.

    lda #0
    sta boxx1
    sta boxy1
    lda #255
    sta boxx2
    lda #255
    sta boxy2
    jsr box

    \ Put BASIC back in the ROM slot to avoid hilarity on exit.

    lda #12
    jsr map_rom
    rts

\ Maps the ROM in A.
.map_rom
    sei
    sta romsel
    sta romsel_ram
    cli
    rts


.box
    \ The line drawing routines don't draw the last pixel, so do that
    \ specially. (We need to probe one pixel anyway so it's no bad
    \ thing.)
    lda boxx2
    sta screenx
    lda boxy2
    sta screeny
    jsr calculate_screen_address
    jsr calculate

    lda pixelcol
    sta corecolour
    stz colourflag

    \ Top stroke

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

    \ Right stroke

    \ screenx, screeny preserved
    lda boxy2
    sec
    sbc boxy1
    sta sidecount
    jsr calculate_screen_address
    jsr vline

    \ Bottom stroke

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

    \ Left stroke

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

    \ Are all the sides the same colour? If so, don't bother recursing.

    lda colourflag
    bne recurse
    jmp floodfill
.recurse

    \ Start recursion.

    lda midx: pha
    lda midy: pha

    \ Calculate centre point.

    lda boxx1
    lsr A
    sta temp
    lda boxx2
    lsr A
    clc
    adc temp
    and #&FE \ round down
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

    \ Recurse into top left.

    lda boxx2: pha
    lda boxy2: pha

    lda midx
    sta boxx2
    lda midy
    sta boxy2
    jsr box

    pla: sta boxy2
    pla: sta boxx2

    \ Recurse into bottom right.

    lda boxx1: pha
    lda boxy1: pha

    lda midx
    sta boxx1
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    pla: sta boxx1

    \ Recurse into bottom left.

    lda boxx2: pha
    lda boxy1: pha
    
    lda midx
    sta boxx2
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    pla: sta boxx2

    \ Recurse into top right.

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


\ Given a screenx/screeny and a calculated screen position, lazily renders the point.
.calculate
    jsr pick
    lda pixelcol
    cmp #&80
    bne dont_calculate

    \ Turns screenx/screeny (0..255, midpoint 0x80) into ci/cr (-2..2).

    lda screenx
    tax
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


\ Once zr, zi, cr, ci have been set up, use reenigne's Mandelbrot kernel to
\ calculate the colour.
.mandel
    lda #ITERATIONS
    sta iterations
.iterator_loop
    ldy #1              \ indexing with this accesses the high byte 

    \ Calculate zr^2 + zi^2. 

    clc
    lda (zr)            \ A = low(zr^2) 
    tax                 
    adc (zi)            \ A = low(zr^2) + low(zi^2) = low(zr^2 + zi^2) 
    sta zr2_p_zi2_lo
    lda (zr), y         \ A = high(zr^2) 
    adc (zi), y         \ A = high(zr^2) + high(zi^2) = high(zr^2 + zi^2) 
    cmp #&08            \ &0800 = 4.0 
    bcs bailout
    sta zr2_p_zi2_hi

    \ Calculate zr + zi. 

    clc
    lda zr+0            \ A = low(zr) 
    adc zi+0            \ A = low(zr + zi) 
    sta zr_p_zi+0
    lda zr+1            \ A = high(zr) 
    adc zi+1            \ A = high(zr + zi) + C 
    and #&3F
    ora #&80            \ fixup 
    sta zr_p_zi+1

    \ Calculate zr^2 - zi^2. 

    txa                 \ A = low(zr^2) 
    sec
    sbc (zi)            \ A = low(zr^2 - zi^2) 
    tax
    lda (zr), y         \ A = high(zr^2) 
    sbc (zi), y         \ A = high(zr^2 - zi^2) 
    sta zr2_m_zi2_hi

    \ Calculate zr = (zr^2 - zi^2) + cr. 

    clc
    txa
    adc cr+0            \ A = low(zr^2 - zi^2 + cr) 
    sta zr+0
]: zr2_m_zi2_hi = P%+1: [OPT pass
    lda #99             \ A = high(zr^2 - zi^2) 
    adc cr+1            \ A = high(zr^2 - zi^2 + cr) 
    and #&3F
    ora #&80            \ fixup 
    sta zr+1

    \ Calculate zi' = (zr+zi)^2 - (zr^2 + zi^2). 

    sec
    lda (zr_p_zi)       \ A = low((zr + zi)^2) 
]: zr2_p_zi2_lo = P%+1: [OPT pass
    sbc #99             \ A = low((zr + zi)^2 - (zr^2 + zi^2)) 
    tax
    lda (zr_p_zi), y    \ A = high((zr + zi)^2) 
]: zr2_p_zi2_hi = P%+1: [OPT pass
    sbc #99             \ A = high((zr + zi)^2 - (zr^2 + zi^2)) 
    tay

    \ Calculate zi = zi' + ci. 

    clc
    txa
]: ci_lo        = P%+1: [OPT pass
    adc #99
    sta zi+0
    tya
]: ci_hi        = P%+1: [OPT pass
    adc #99
    and #&3F
    ora #&80            \ fixup 
    sta zi+1

    dec iterations
    bne iterator_loop

.bailout
]: iterations   = P%+1: [OPT pass
    lda #99
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


\ Fill the current box with pixelcol.
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


\ Loads screenptr with the address of the pixel at screenx/screeny.
.calculate_screen_address
    lda screenx
    lsr A \ to physical pixels
    lsr A \ to pixels/2
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


\ Given a calculated screenptr, moves to the next horizontal physical pixel
\ (which is two logical pixels because MODE 2).
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


\ Given a calculated screenptr, moves to the next vertical pixel.
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

]: rowsize = 640 - 8: [OPT pass
    clc
    lda screenptr+0
    adc #rowsize MOD 256
    sta screenptr+0
    lda screenptr+1
    adc #rowsize DIV 256
    sta screenptr+1

.go_to_pixel_down_exit
    rts


\ Plot colour pixelcol to the pixel at screenx/screeny (calculate_screen_address must
\ have been called). Corrupts pixelcol!
.plot:
    lda #&55
    sta pixelmask

    \ Unshifted values refer to the *left* hand pixel, so odd pixels
    \ need adjusting.
    lda screenx     \ Is this an even pixel?
    ror A
    ror A           \ odd/even bit to C
    bcc plot_even_pixel

    lsr pixelcol
    asl pixelmask

.plot_even_pixel
    lda (screenptr)
    and pixelmask
    ora pixelcol
    sta (screenptr)
    rts


\ Pick colour from screenx/screeny (calculate_screen_address must have been
\ called) into pixelcol.
.pick
    lda screenx
    ror A
    ror A \ odd/even bit to C
    lda (screenptr)
    \ Unshifted values refer to the *left* hand pixel, so odd pixels
    \ need adjusting.
    bcc pick_even_pixel
    asl A
.pick_even_pixel
    and #&AA
    sta pixelcol
    rts


\ Resets the screen.
.init_screen
    lda #22: jsr oswrch
    lda #2: jsr oswrch

    lda #39: sta &30A             \ Characters per line
    lda #16: sta &34F             \ Byte per character

    ldx #0
.init_screen_loop
    lda setup_bytes, X
    jsr oswrch
    inx
    cpx #(setup_bytes_end - setup_bytes)
    bne init_screen_loop
    rts

.setup_bytes
    equb 23    \ redefine
    equb 1     \ cursor
    equb 0     \ off
    equd 0: equd 0
    equb 19    \ redefine palette
    equb 8     \ special marker colour
    equb 0     \ ...to black
    equw 0: equb 0
    equb 17    \ set colour
    equb 128+8 \ special marker colour
.setup_bytes_end


\ Clears the screen between renders.
.clear_screen
    lda #12
    jmp oswrch


\ Build the pixels-to-z table.
.build_pixels_to_z_table
    lda left+0
    sta zr+0
    lda left+1
    sta zr+1

    lda top+0
    sta zi+0
    lda top+1
    sta zi+1

    ldx #0
.build_pixels_to_z_loop
    clc
    lda zr+0
    sta pixels_to_zr_lo, X
    adc step
    sta zr+0

    lda zr+1
    sta pixels_to_zr_hi, X
    adc #0
    and #&3F
    ora #&80
    sta zr+1

    clc
    lda zi+0
    sta pixels_to_zi_lo, X
    adc step
    sta zi+0

    lda zi+1
    sta pixels_to_zi_hi, X
    adc #0
    and #&3F
    ora #&80
    sta zi+1

    inx
    bne build_pixels_to_z_loop
    rts


\ Build the column table (pixels/2 to address offset).
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


\ Build the row table (pixels to address).
.build_row_table
    stz screenptr+0
    lda #&30 \ framebuffer at &3000
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

    \ Reached the end of a char row; increment by (640-8) to move
    \ to the next char row.

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


.set_charset
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
    lsr A               \ odd/even bit to C
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
    lsr A               \ odd/even bit to C
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


\ Maps logical colours (0..15) to MODE 2 left-hand-pixel values.
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

.charset
]: charset_o = O%: P% = P% + 896: O% = O% + 896: [OPT pass
]
program_end = O%
program_size = program_end - program_start

pixels_to_zr_lo = P%+&0000
pixels_to_zr_hi = P%+&0100
pixels_to_zi_lo = P%+&0200
pixels_to_zi_hi = P%+&0300
col_table_lo    = P%+&0400
col_table_hi    = P%+&0480: REM 128 bytes onle
row_table_lo    = P%+&0500
row_table_hi    = P%+&0600

ENDPROC

DEF FNtofixed(f) = (f * fixedmul) AND &3FFF OR &8000

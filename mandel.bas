
HIMEM = &7000
program_buffer = &7000
buffer_size = &1000
program_start = 0
program_end = 0
program_size = 0
kernel_start = 0
kernel_end = 0
kernel_size = 0

oswrch     = &FFEE
romsel     = &FE30
romsel_ram = &F4
run_location = &E00

ITERATIONS = 32

PRINT "pass 1": PROCassemble(4)
PRINT "pass 2": PROCassemble(6)
PRINT "pass 3": PROCassemble(6)

PRINT "Total size: &"; ~(O% - program_buffer)
PRINT "...main:    &"; ~(program_end - program_start)
PRINT "...kernel:  &"; ~(kernel_end - kernel_start)

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

.zi             equw 0
.zr             equw 0
.zi2            equw 0
.zr2            equw 0
.zr_p_zi        equw 0
.iterations     equb 0
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
    \ First, initialise the screen.
    ldx #0
.init_screen_loop
    lda setup_bytes, X
    jsr oswrch
    inx
    cpx #(setup_bytes_end - setup_bytes)
    bne init_screen_loop

    \ Map sideways RAM bank 4, containing our lookup table.

    lda #4
    jsr map_rom

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
    ldx screeny
    lda row_table_lo, X
    sta screenptr+0
    lda row_table_hi, X
    sta screenptr+1

    \ Calculate the X offset (into pixelmask/A)
    lda screenx     \ remember these are logical pixels (0..255)
    and #&FC        \ mask to char column
    stz pixelmask   \ reuse this as high byte of X offset
    asl A           \ *2
    rol pixelmask
    
    \ Add on the X offset.
    \ C is already clear
    tax
    adc screenptr+0
    sta screenptr+0
    lda screenptr+1
    adc pixelmask
    sta screenptr+1
    txa

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

.setup_bytes
    equb 22    \ mode
    equb 2
    equb 19    \ redefine palette
    equb 8     \ special marker colour
    equb 0     \ ...to black
    equw 0: equb 0
    equb 28    \ set text window
    equb 0: equb 31: equb 15: equb 0
    equb 17    \ set colour
    equb 128+8 \ special marker colour
    equb 12    \ clear window
    equb 28    \ set text window
    equb 16: equb 31: equb 19: equb 0
    equb 17    \ set colour
    equb 128   \ black
    equb 12    \ clear window
.setup_bytes_end


\ The row lookup tables; maps character rows to screen addresses.
.row_table_lo:
]
FOR Y%=0 TO 255
    [OPT pass
        equb (&3000 + (Y% DIV 8)*640 + (Y% MOD 8)) MOD 256
    ]
NEXT
[OPT pass
.row_table_hi:
]
FOR Y%=0 TO 255
    [OPT pass
        equb (&3000 + (Y% DIV 8)*640 + (Y% MOD 8)) DIV 256
    ]
NEXT

kernel_start = O%
[OPT pass
\ Actually do the work of calculating the colour of a pixel (in screenx, screeny).
.mandel
    \ Turns screenx/screeny (0..255, midpoint 0x80) into ci/cr (-2..2).

    lda screenx
    ldx #0
    clc
    adc #&80            \ adjust midpoint 
    bpl x_not_negative
    dex                 \ if negative, sign extend high byte 
.x_not_negative
    asl A               \ the number in cr+1.A is now -0.5..0.5, so double 
    sta cr_lo
    txa      
    rol A
    asl cr_lo           \ and again 
    rol A
    clc
    adc #&00            \ X offset 
    and #&3F            \ fixup the high byte to be an address 
    ora #&80
    sta cr_hi
    sta zr+1
    lda cr_lo
    sta zr+0

    ldx #0
    lda screeny
    clc
    adc #&80            \ adjust midpoint 
    bpl y_not_negative
    dex                 \ if negative, sign extend high byte 
.y_not_negative
    asl A               \ the number in ci+1.A is now -1..1, so double 
    sta ci_lo
    txa      
    rol A
    asl ci_lo           \ and again 
    rol A
    clc
    adc #&00            \ Y offset 
    and #&3F            \ fixup the high byte to be an address 
    ora #&80
    sta ci_hi
    sta zi+1
    lda ci_lo
    sta zi+0

    \ Now we go into reenigne's Mandelbrot kernel. 

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
    sta zr2_m_zi2_lo
    lda (zr), y         \ A = high(zr^2) 
    sbc (zi), y         \ A = high(zr^2 - zi^2) 
    sta zr2_m_zi2_hi

    \ Calculate zr = (zr^2 - zi^2) + cr. 

    clc
]: zr2_m_zi2_lo = P%+1: [OPT pass
    lda #99             \ A = low(zr^2 - zi^2) 
]: cr_lo        = P%+1: [OPT pass
    adc #99             \ A = low(zr^2 - zi^2 + cr) 
    sta zr+0
]: zr2_m_zi2_hi = P%+1: [OPT pass
    lda #99             \ A = high(zr^2 - zi^2) 
]: cr_hi        = P%+1: [OPT pass
    adc #99             \ A = high(zr^2 - zi^2 + cr) 
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
    lda iterations
    and #7
    tax
    lda palette, X
    sta pixelcol
    rts
]
kernel_end = O%
program_end = O%
program_size = program_end - program_start
kernel_size = kernel_end - kernel_start

ENDPROC

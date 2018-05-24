ITERATIONS = 32

top = 1
left = -1
right = 1
bottom = -1

width = right - left
height = bottom - top
xstep = width / 256
ystep = height / 256

fixedmul = 2^9

HIMEM = &7000
program_buffer = &7000
buffer_size = &1000
program_start = 0
program_end = 0
program_size = 0
kernel_start = 0
kernel_end = 0
kernel_size = 1

oswrch     = &FFEE
romsel     = &FE30
romsel_ram = &F4
run_location = &E00

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
P% = &72
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

    \ Copy the kernel.

    ldx #kernel_size-1
.copy_kernel_loop
    lda kernel_start, X
    sta mandel, X
    dex
    bpl copy_kernel_loop

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

    \ Turns screenx/screeny (0..255, midpoint 0x80) into ci/cr (-2..2).

    lda screenx
    lsr A
    tax
    lda pixels_to_reals_lo, X
    sta cr_lo
    sta zr+0
    lda pixels_to_reals_hi, X
    sta cr_hi
    sta zr+1

    ldy screeny
    lda pixels_to_imaginary_lo, Y
    sta ci_lo
    sta zi+0
    lda pixels_to_imaginary_hi, Y
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

[OPT pass
\ The column lookup tables; maps physical pixels/2 to scanline offsets.
.col_table_lo
]
FOR X%=0 TO 255 STEP 2
    [OPT pass
        equb (X% * 4) MOD 256
    ]
NEXT
[OPT pass
.col_table_hi
]
FOR X%=0 TO 255 STEP 2
    [OPT pass
        equb (X% * 4) DIV 256
    ]
NEXT

[OPT pass
\ Maps logical X pixels/2 to fixed-point reals.
.pixels_to_reals_lo
]
FOR X%=0 TO 255 STEP 2
    [OPT pass
        equb FNtofixed(X%*xstep + left) MOD 256
    ]
NEXT
[OPT pass
.pixels_to_reals_hi
]
FOR X%=0 TO 255 STEP 2
    [OPT pass
        equb FNtofixed(X%*xstep + left) DIV 256
    ]
NEXT

[OPT pass
\ Maps logical Y pixels to fixed-point imaginaries.
.pixels_to_imaginary_lo
]
FOR Y%=0 TO 255
    [OPT pass
        equb FNtofixed(Y%*ystep + top) MOD 256
    ]
NEXT
[OPT pass
.pixels_to_imaginary_hi
]
FOR Y%=0 TO 255
    [OPT pass
        equb FNtofixed(Y%*ystep + top) DIV 256
    ]
NEXT
program_end = O%

kernel_start = O%
P% = 0
[OPT pass
\ Once zr, zi, cr, ci have been set up, use reenigne's Mandelbrot kernel to
\ calculate the colour.
.mandel
    lda #ITERATIONS
    sta iterations
.iterator_loop
    ldy #1              \ indexing with this accesses the high byte 

    \ Calculate zr^2 + zi^2. 

    clc
]: zr = P%+1: [OPT pass
    lda &9999           \ A = low(zr^2) 
    tax                 
]: zi = P%+1: [OPT pass
    adc &9999           \ A = low(zr^2) + low(zi^2) = low(zr^2 + zi^2) 
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
]: zr_p_zi = P%+1: [OPT pass
    lda &9999           \ A = low((zr + zi)^2) 
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
]
kernel_end = O%
program_size = program_end - program_start
kernel_size = kernel_end - kernel_start

ENDPROC

DEF FNtofixed(f) = (f * fixedmul) AND &3FFF OR &8000

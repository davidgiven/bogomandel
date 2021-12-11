ITERATIONS = 32

fraction_bits = 11 ; including the bottom 0
integer_bits = 4
total_bits = fraction_bits + integer_bits

oswrch     = &FFEE
osbyte     = &FFF4
accon      = &FE34
romsel     = &FE30
romsel_ram = &F4
evntv      = &220
sheila     = &fe00
ifr        = 13
ier        = 14

system_via = &40
kbd_irq    = 1<<0
vsync_irq  = 1<<1
adc_irq    = 1<<4
clock_irq  = 1<<6

accon_x    = 4 ; ACCON bit which maps shadow RAM into the address space

zp_start = &a8 ; we stomp over the transient command and filesystem workspace
mc_base = &2000
mc_top  = &3000

cpu 1 ; 65C02

puttext "src/!boot", "!boot", 0
putbasic "src/loader.bas", "loader"
putbasic "src/shell.bas", "shell"

; --- Global page ------------------------------------------------------------

org zp_start
.centerx        equw 0
.centery        equw 0
.step           equb 0
.julia          equb 0
.cr             equw 0
.ci             equw 0
.clock          equw 0

.screeny        equb 0

.boxx1          equb 0
.boxy1          equb 0
.boxx2          equb 0 ; INCLUSIVE (so 0..255)
.boxy2          equb 0 ; INCLUSIVE (so 0..255)
.midx           equb 0
.midy           equb 0
.sidecount      equb 0

.colourflag     equb 0
.exitflag       equb 0
.scroll         equb 0
print "zero page:", ~zp_start, "to", ~P%

; --- The kernel ------------------------------------------------------------

; Once zr, zi, cr, ci have been set up, use reenigne's Mandelbrot kernel to
; calculate the colour. This is copied into zero page when we need it
; (preserving Basic's workspace for use later).
;
; This routine also contains the code which updates colourflag and draws the
; pixel; this allows us to inline a lot of variables for speed and use a
; very cunning trick (suggested by rtw) for using tsb for the read/modify/
; write operation to put the pixel on the screen.

    org &0
    guard &a0
.kernel
    ; Map the lookup tables.
    lda #accon_x
    trb accon
    
    lda #ITERATIONS
    sta iterations
.iterator_loop
    ldy #1              ; indexing with this accesses the high byte 

    ; Calculate zr^2 + zi^2. 

    clc
zr = *+1
    lda 9999            ; A = low(zr^2) 
    tax                 
zi = *+1
    adc 9999            ; A = low(zr^2) + low(zi^2) = low(zr^2 + zi^2) 
    sta zr2_p_zi2_lo
    lda (zr), y         ; A = high(zr^2) 
    adc (zi), y         ; A = high(zr^2) + high(zi^2) = high(zr^2 + zi^2) 
    sta zr2_p_zi2_hi
    and #&7f
    cmp #4 << (fraction_bits-8)
    bcs bailout

    ; Calculate zr + zi. 

    ; we know C is unset from the bcs above
    lda zr+0            ; A = low(zr) 
    adc zi+0            ; A = low(zr + zi) 
    sta zr_p_zi+0
    lda zr+1            ; A = high(zr) 
    adc zi+1            ; A = high(zr + zi) + C 

    ; Both X and Y are in use, so we use self-modifying code to avoid needing
    ; to disturb them. One cycle longer that tax; lda fixup_table, X.
    sta lowbyteoftable
lowbyteoftable = * + 1
    equb &ad ; lda abs
    equw fixup_table
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
kernel_cr_lo = *+1
    adc #99             ; A = low(zr^2 - zi^2 + cr) 
    sta zr+0
zr2_m_zi2_hi = *+1
    lda #99             ; A = high(zr^2 - zi^2) 
kernel_cr_hi = *+1
    adc #99             ; A = high(zr^2 - zi^2 + cr) 
    tax
    equb &bd: equw fixup_table ; lda fixup_table, X
    sta zr+1

    ; Calculate zi' = (zr+zi)^2 - (zr^2 + zi^2). 

    sec
zr_p_zi = *+1
    lda 9999            ; A = low((zr + zi)^2) 
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
kernel_ci_lo = *+1
    adc #99
    sta zi+0
    tya
kernel_ci_hi = *+1
    adc #99
    tax
    equb &bd: equw fixup_table ; lda fixup_table, X
    sta zi+1

    dec iterations
    bne iterator_loop

.bailout
    ; Unmap the lookup tables.
    lda #accon_x
    tsb accon
    
    ; We've finished with the calculation; update the colour flag and draw
    ; the pixel on the screen.

iterations = * + 1
    equb &ae ; ldx abs
    equw palette ; low byte is patched with iterations byte
corecolour = * + 1
    cpx #99
{
    beq skip
    stz colourflag
.skip
}

    ; Plot colour A to the current pixel.

    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
screenx = * + 1
    lda #99         ; Is this an even pixel?
    ror A           ; odd/even bit to C
    txa
    bcc plot_even_pixel
    lsr A
.plot_even_pixel
screenptr = * + 1
    tsb &9999       ; unset pixels guaranteed to be 0

    ; pixel colour in A on exit
    txa
    rts
.kernel_end

kernel_size = kernel_end - kernel
print "Kernel:", ~kernel_size

; --- Main program ----------------------------------------------------------

; Given a screen address in X and Y, updates screenptr.
macro calculate_screen_address
    clc
    lda row_table_lo, Y
    adc col_table_lo, X
    sta screenptr+0
    lda row_table_hi, Y
    adc col_table_hi, X
    sta screenptr+1
endmacro

; Lazily renders the current point, leaving the colour in A.
macro calculate_through_cache
    ; Poll the clock.

    bit sheila+system_via+ifr
    bvc noclock

    ; Acknowledge interrupt
    lda #clock_irq
    sta sheila+system_via+ifr

    ; Increment clock counter
    inc clock+0
    bne noclock
    inc clock+1
    
.noclock
    ; Pick colour from screenx/screeny (calculate_screen_address must have been
    ; called) into A.

    lda screenx
    ror A ; odd/even bit to C
    lda (screenptr)
    ; Unshifted values refer to the *left* hand pixel, so odd pixels
    ; need adjusting.
    bcc pick_even_pixel
    asl A
.pick_even_pixel
    bmi dont_calculate
    jsr recalculate_pixel
    bra exit

.dont_calculate
    ; This pixel is cached, so just check the colour and exit.
    and #&AA ; mask out the other pixel
    cmp corecolour
    beq exit
    stz colourflag
.exit
endmacro


clear mc_base, mc_top
org mc_base
guard mc_top

    ; ...and this gets invoked once the program's successfully copied.

    ; Initialisation.

.main_program_start
    jsr kernel_inout
    jsr build_row_table
    jsr build_col_table

    ; Map sideways RAM bank 4, containing our lookup table.

    lda #4
    jsr map_rom

    ; Map the framebuffer.

    lda #accon_x
    tsb accon

    jsr init_screen

    ; Copy input parameters into the kernel.

    lda cr+0
    sta kernel_cr_lo
    lda cr+1
    sta kernel_cr_hi
    lda ci+0
    sta kernel_ci_lo
    lda ci+1
    sta kernel_ci_hi

    ; Compute zoom table.

    jsr build_pixels_to_z_table

    ; Turn interrupts off entirely; we'll keep track of time and keyboard by polling
    ; the system VIA directly.

    sei
    stz clock+0
    stz clock+1

    ; Draw.

    lda #0
    sta boxx1
    sta boxy1
    lda #127
    sta boxx2
    lda #255
    sta boxy2
    jsr box

    ; Put things back the way they were.

    cli

    lda #accon_x
    trb accon
    lda #12
    jsr map_rom
    jsr kernel_inout
    rts

; Maps the ROM in A.
.map_rom
    sei
    sta romsel
    sta romsel_ram
    cli
.handy_rts
    rts


.box
{
    ; Hacky temporary storage, reusing zr and zi in the kernel.
    boxx1i = zr+0
    boxy1i = zr+1
    boxx2i = zi+0
    linewidth = zi+1

    ; Check for keypress.

    lda #kbd_irq
    bit sheila+system_via+ifr
    bne handy_rts

    ; *** First: draw the outlines of the box *******************************

    ; The line drawing routines don't draw the last pixel, so do that
    ; specially. (We need to probe one pixel anyway so it's no bad
    ; thing.)

    ldx boxx2
    stx screenx
    ldy boxy2
    sty screeny
    calculate_screen_address
    calculate_through_cache
    sta corecolour
    lda #&ff
    sta colourflag

    ; Top stroke

    ldx boxx1
    stx screenx
    ldy boxy1
    sty screeny
    sec
    lda boxx2
    sbc boxx1
    sta sidecount
    calculate_screen_address
    jsr hline

    ; Right stroke

    ; screenx, screeny point at RHS of stroke
    sec
    lda boxy2
    sbc boxy1
    sta sidecount
    jsr vline

    ; Left stroke

    ldx boxx1
    stx screenx
    ldy boxy1
    sty screeny
    sec
    lda boxy2
    sbc boxy1
    sta sidecount
    calculate_screen_address
    jsr vline

    ; Bottom stroke

    ; screenx, screeny point at bottom of stroke
    sec
    lda boxx2
    sbc boxx1
    sta sidecount
    jsr hline

    ; If the sides aren't all the same colour, recurse.

    bit colourflag
    bpl recurse

    ; Otherwise, fill the current box.

    ; *** Then either: fill the current box *********************************

    ; The margins of the box are already drawn. We can use this to avoid
    ; the (expensive) cost of having to draw stray pixels on the left and
    ; right, at the expense of a (very cheap) overdraw.

    lda boxx1
    bit #1
    beq left_margin_even
    inc A
.left_margin_even
    sta boxx1i

    lda boxx2
    bit #1
    bne right_margin_odd
    dec A
.right_margin_odd
    sta boxx2i

    ; Calculate length of line.

    sec
    ;lda boxx2i ; left in A from previous instruction
    sbc boxx1i
    beq exit ; don't do anything if the box has zero width
    cmp #32
    bcs recurse ; always recurse if the box is bigger than 32
    and #&7e ; round down; now it's 2* bytes
    asl A ; to 4* bytes
    asl A ; to 8* bytes
    sta linewidth
    
    ; Compute pixel colour.

    lda corecolour
    lsr A
    ora corecolour
    sta corecolour

    ; The top bound is *inclusive*, and we don't want to redraw the top row.
    ; The bottom bound is *exclusive* (as it makes the comparison cheaper)
    ; and so we leave it in boxy2 and don't copy it.

    lda boxy1
    inc A
    sta boxy1i
    sta screeny
    
    ; Check that our box does not have zero height.

    lda boxy2
    dec A
    cmp boxy1i
    beq exit 

.yloop
    ldx boxx1i
    ldy screeny
    calculate_screen_address

    ldy linewidth
    ldx corecolour
.xloop
    ; displacement in Y on entry
    txa
    sta (screenptr), Y
    tya
    sec
    sbc #8
    tay
    bcs xloop ; value passed zero

    inc screeny
    lda screeny
    cmp boxy2
    bcc yloop
.exit
    rts

    ; *** Or: recurse into the current box **********************************

    ; Start recursion. First, calculate the centre point, pushing as we go.
.recurse
    clc
    lda boxx1
    adc boxx2 ; produces 9-bit result in C:A
    ror A     ; 9-bit right shift
    cmp boxx1
    beq box_too_small_x

    ldx midx
    phx
    sta midx

    clc
    lda boxy1
    adc boxy2 ; produces 9-bit result in C:A
    ror A     ; 9-bit right shift
    cmp boxy1
    beq box_too_small_y

    ldy midy
    phy
    sta midy

    ; Recurse into top left.

    lda boxx2: pha
    lda boxy2: pha

    lda midx
    sta boxx2
    lda midy
    sta boxy2
    jsr box

    pla: sta boxy2
    ;pla: sta boxx2 --- immediately pushed back

    ; Recurse into bottom left.

    ;lda boxx2: pha
    lda boxy1: pha
    
    ;lda midx --- already in boxx2.
    ;sta boxx2
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    plx: stx boxx2

    ; Recurse into bottom right.

    ldx boxx1: phx
    ;lda boxy1 ; already in A from previous pull
    pha

    lda midx
    sta boxx1
    lda midy
    sta boxy1
    jsr box

    pla: sta boxy1
    ;pla: sta boxx1 --- immediately pushed back

    ; Recurse into top right.

    ;lda boxx1: pha
    lda boxy2: pha

    ;lda midx --- midx already in boxx1.
    ;sta boxx1
    lda midy
    sta boxy2
    jsr box

    pla: sta boxy2
    pla: sta boxx1

    pla: sta midy
.box_too_small_y
    pla: sta midx
.box_too_small_x

    rts
}


; Runs the kernel to calculate the current pixel, and draws it.
.recalculate_pixel
{
    ; Turns screenx (0..127) / screeny (0..255) into ci/cr (-2..2).
    lda julia
    bne setup_julia

    ; Mandelbrot setup: x/y -> zr, cr, zi, cr
    ldx screenx
    lda pixels_to_zr_lo, X
    sta zr+0
    sta kernel_cr_lo
    lda pixels_to_zr_hi, X
    sta zr+1
    sta kernel_cr_hi

    ldy screeny
    lda pixels_to_zi_lo, Y
    sta zi+0
    sta kernel_ci_lo
    lda pixels_to_zi_hi, Y
    sta zi+1
    sta kernel_ci_hi

    jmp kernel

.setup_julia
    ; Julia setup: x/y -> zr, zi; leave cr, ci unchanged
    ldx screenx
    lda pixels_to_zr_lo, X
    sta zr+0
    lda pixels_to_zr_hi, X
    sta zr+1

    ldy screeny
    lda pixels_to_zi_lo, Y
    sta zi+0
    lda pixels_to_zi_hi, Y
    sta zi+1

    jmp kernel
}


align &100 ; hacky, but prevents page transitions in the code
.hline
{
    calculate_through_cache

    ; Moves to the next horizontal pixel.

    inc screenx
    lda screenx
    ror A
    bcs next

    ; This pixel is even, so move to the next char.

    ; (C unset due to bcs above)
    lda screenptr+0
    adc #8
    sta screenptr+0
    bcs inchighbyte
.next
    dec sidecount
    bne hline
    rts

.inchighbyte
    inc screenptr+1
    bra next
}


.vline
{
    calculate_through_cache

    ; Move to the next vertical pixel.

    inc screenptr+0
    beq inchighbyte

.noinc
    inc screeny
    lda screeny
    and #7
    beq nextrow
    dec sidecount
    bne vline
    rts

.inchighbyte
    inc screenptr+1
    bra noinc

.nextrow
    clc
    lda screenptr+0
    adc #lo(640-8)
    sta screenptr+0
    lda screenptr+1
    adc #hi(640-8)
    sta screenptr+1

    dec sidecount
    bne vline
    rts
}


; Swap the kernel to/from zero page, preserving Basic's state.
.kernel_inout
{
    ldx #kernel_size-1
.loop
    lda kernel, X
    ldy kernel_data, X
    sta kernel_data, X
    sty kernel, X
    dex
    cpx #&ff
    bne loop
    rts
}


; Clears the screen between renders.
.init_screen
    ldx scroll
    jmp (scrolltable,x)
.scrolltable
    equw clear_screen
    equw scroll_left
    equw scroll_right
    equw scroll_down
    equw scroll_up

.clear_screen
{
    ldy #0
.yloop
    ldx #0
.xloop
    calculate_screen_address
    lda #0
    sta (screenptr)
    inx
    inx
    cpx #128
    bne xloop
    iny
    bne yloop
    rts
}

; Move the contents of the screen up 64 rows, as in response to down-arrow
.scroll_down
{
    ldy #64
.yloop
    ldx #0
.xloop
    calculate_screen_address
    phy
    lda (screenptr)
    pha
    tya
    sec
    sbc #64
    tay
    calculate_screen_address
    pla
    sta (screenptr)
    ply
    inx
    inx
    cpx #128
    bne xloop
    iny
    bne yloop
}
{
    ldy #192
.yloop
    ldx #0
.xloop
    calculate_screen_address
    lda #0
    sta (screenptr)
    inx
    inx
    cpx #128
    bne xloop
    iny
    bne yloop
    rts
}

; Move the contents of the screen down 64 rows, as in response to up-arrow
.scroll_up
{
    ldy #191
.yloop
    ldx #0
.xloop
    calculate_screen_address
    phy
    lda (screenptr)
    pha
    tya
    clc
    adc #64
    tay
    calculate_screen_address
    pla
    sta (screenptr)
    ply
    inx
    inx
    cpx #128
    bne xloop
    dey
    cpy #255
    bne yloop
}
{
    ldy #0
.yloop
    ldx #0
.xloop
    calculate_screen_address
    lda #0
    sta (screenptr)
    inx
    inx
    cpx #128
    bne xloop
    iny
    cpy #64
    bne yloop
    rts
}

; Move the contents of the screen left 64 columns, as in response to right-arrow
.scroll_right
{
    ldx #32
.xloop
    ldy #0
.yloop
    calculate_screen_address
    phx
    lda (screenptr)
    pha
    txa
    sec
    sbc #32
    tax
    calculate_screen_address
    pla
    sta (screenptr)
    plx
    iny
    bne yloop
    inx
    inx
    cpx #128
    bne xloop
}
{
    ldy #0
.yloop
    ldx #96
.xloop
    calculate_screen_address
    lda #0
    sta (screenptr)
    inx
    inx
    cpx #128
    bne xloop
    iny
    bne yloop
    rts
}

; Move the contents of the screen right 64 rows, as in response to left-arrow
.scroll_left
{
    ldx #95
.xloop
    ldy #0
.yloop
    calculate_screen_address
    phx
    lda (screenptr)
    pha
    txa
    clc
    adc #32
    tax
    calculate_screen_address
    pla
    sta (screenptr)
    plx
    iny
    bne yloop
    dex
    dex
    cpx #255
    bne xloop
}
{
    ldy #0
.yloop
    ldx #0
.xloop
    calculate_screen_address
    lda #0
    sta (screenptr)
    inx
    inx
    cpx #32
    bne xloop
    iny
    bne yloop
    rts
}

; Build the pixels-to-z table.
.build_pixels_to_z_table
{
temp = screenptr ; hacky temporary storage

    ; Load temp with step*128 (half a screen width).

    stz temp+0
    lda step ; A:(temp+0) = step * 256

    lsr A
    ror temp+0 ; A:(temp+0) = step * 128
    sta temp+1

    ; Now set zr and zi to the top and left of the image.

    sec
    lda centerx+0
    sbc temp+0
    sta zr+0
    lda centerx+1
    sbc temp+1
    sta zr+1

    sec
    lda centery+0
    sbc temp+0
    sta zi+0
    lda centery+1
    sbc temp+1
    sta zi+1

    ; Y pixels go from 0 to 255, with 0x80 being the midpoint.

    ldy #0
.yloop
    clc
    lda zi+0
    sta pixels_to_zi_lo, Y
    adc step
    sta zi+0

    ldx zi+1
    lda fixup_table, X
    sta pixels_to_zi_hi, Y
    adc #0
    sta zi+1

    iny
    bne yloop
    ; (leaves Y=0)

    ; X pixels go from 0 to 127, with 0x40 being the midpoint, using double the step.

    rol step

    ; Y is 0 from the bne above
.xloop
    clc
    lda zr+0
    sta pixels_to_zr_lo, Y
    adc step
    sta zr+0

    ldx zr+1
    lda fixup_table, X
    sta pixels_to_zr_hi, Y
    adc #0
    sta zr+1

    iny
    bpl xloop ; exit at x=128

    ror step ; remember to put step back the way it was!
    rts
}


; Build the column table (bytes to address offset).
.build_col_table
{
    stz screenptr+0
    stz screenptr+1
    ldx #0
.loop
    clc
    lda screenptr+0
    sta col_table_lo+0, X
    sta col_table_lo+1, X
    adc #8
    sta screenptr+0

    lda screenptr+1
    sta col_table_hi+0, X
    sta col_table_hi+1, X
    adc #0
    sta screenptr+1

    inx
    inx
    bpl loop ; loop until 127
    rts
}


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


.kernel_data
    skip kernel_size
    copyblock kernel, kernel_end, kernel_data

; Used to fixup the high byte of a number, so that it's a valid pointer to its
; own square in the lookup table. The table runs from &4000 to &C000, and the
; rule is that bit7 = !bit6.
align &100 ; must be page aligned
.fixup_table
    for i, 0, 255
        equb (i and &7f) or (i and &40 eor &40)<<1
    next

; Maps logical colours (0..15) to MODE 2 left-hand-pixel values.
align &100 ; must be page aligned
.palette
    equb &80 ; high-bit colours 8-15
    equb &82
    equb &88
    equb &8A
    equb &A0
    equb &A2
    equb &A8
    equb &AA
    ; These colours are used for looking up iterations. They're just repeated
    ; copies of the high-bit colours, black excluded (because black stripes
    ; look ugly).
    for i, 0, 2
        equb &82
        equb &88
        equb &8A
        equb &A0
        equb &A2
        equb &A8
        equb &AA
    next
    equb &82 ; stray
    equb &88 ; stray
    equb &8A ; stray
    assert (* - palette) = ITERATIONS
    equb &A0 ; for points *outside* the set

    
.main_program_end

; Uninitialised data follows.

align &100
.pixels_to_zi_lo    skip &100
.pixels_to_zi_hi    skip &100
.row_table_lo       skip &100 ; pixels; 0..255
.row_table_hi       skip &100
.pixels_to_zr_lo    skip &80
.pixels_to_zr_hi    skip &80
.col_table_lo       skip &80 ; pixels; 0..255
.col_table_hi       skip &80

print "mandel:", ~main_program_start, "to", ~main_program_end, "data top:", ~P%
save "mandel", main_program_start, main_program_end

; --- Screen mode setup -----------------------------------------------------

clear mc_base, mc_top
org mc_base
guard mc_top

.setscreen_start
{
    lda #22: jsr oswrch
    lda #129: jsr oswrch  ; Note mode 129 (so text windows work)

    lda #39:  sta &30A    ; Characters per line
    lda #16:  sta &34F    ; Bytes per character
    lda #&0F: sta &360    ; Number of colours
    lda #&02: sta &361    ; Pixels per byte
    lda #&AA: sta &362    ; Pixel left mask
    lda #&55: sta &363    ; Pixel right mask
    lda #154: ldx #&F4: jsr osbyte ; Video ULA control register

{
    ldx #0
.loop
    lda setup_bytes, X
    jsr oswrch
    inx
    cpx #(setup_bytes_end - setup_bytes)
    bne loop
}

    ; Load jgh's special thin character set.

    lda #charset MOD 256
    sta screenptr+0
    lda #charset DIV 256
    sta screenptr+1

{
    ldx #0
.loop
    lda #19
    jsr oswrch
    txa
    ora #8
    jsr oswrch
    txa
    jsr oswrch
    lda #0
    jsr oswrch
    jsr oswrch
    jsr oswrch
    inx
    cpx #8
    bne loop
}

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
    bcc dont_advance
    inc screenptr+1

.dont_advance
    inx
    cpx #127
    bne charloop
    rts

.setup_bytes
    equb 20                            ; reset palette
    equb 23, 1, 0, 0, 0, 0, 0, 0, 0, 0 ; cursor off
    equb 24: equw 0, 0, 1024, 1024     ; set graphics window
.setup_bytes_end

.charset
    incbin "data/ThinSet"
}
.setscreen_end

print "setscrn:", ~setscreen_start, "to", ~setscreen_end
save "setscrn", setscreen_start, setscreen_end

; --- Table of squares ------------------------------------------------------

org &4000
guard &c000

.squares_start
{
    for i, 0, (1<<total_bits)-1, 2
        if i >= (1<<total_bits)/2
            extended = i - (1<<total_bits)
        else
            extended = i
        endif
        real = extended / (1<<fraction_bits)
        square = real^2

        ; Calculate the address of this number (taking into account the fixup).
        if extended and &4000
            address = extended and &7ffe
        else
            address = extended and &7ffe or &8000
        endif

        ; Clamp the result at MAXINT.
        if square > (1<<integer_bits)/2
            clampedsquare = (1<<integer_bits)/2 - 1/(1<<fraction_bits)
        else
            clampedsquare = square
        endif

        ; result is a square, and so is always positive! So we need to lose
        ; the sign bit.
        result = (clampedsquare * (1<<fraction_bits)) and &3ffe or &8000

        ;print real, ~address, square, ~result

        org address
        equw result
    next
}
.squares_end

save "squaren", &4000, &8000
save "squarep", &8000, &c000

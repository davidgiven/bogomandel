# Bogomandel

A stupid mandelbrot program. v0, 2018-05-21

## What?

This is a really hacky toy that I hacked up. It draws a sort-of Mandelbrotish
image on a BBC Micro in about 12 seconds.

## How?

To build, you need a Linux with the xa65 assembler. Do `make` and it'll build
a `mandel.ssd` disk image which you can boot (in, say, jsbeeb).

You will need a BBC Master 128, as this uses all the sideways RAM.

## Why?

Why not?

This is a toy, largely stolen from http://blog.nothinguntoward.eu/?p=38; I
changed it to use 2.6 fixed point arithmetic, fixed-point multiplications via
an enormous 64kB lookup table, recursive subdivision to allow quick filling
of large areas, and the top/bottom mirroring. As 2.6 fixed point fits in a
byte, I was curious to know whether it would work adequately on a BBC Micro.
It does.

## Why not?

This is a toy; it can generate this one image only. You can't zoom (because
there's not enough precision in the number representation). You can't pan
(because it's limited to drawing the box between +/- root 2).

It could be made faster, mostly by using more intelligent flood filling and
avoiding having to trace the entire bounds of each square, but it's probably
not worth it.

## License?

Two-clause BSD; see the COPYING file. Go nuts.

## Who?

This was written by me, David Given <dg@cowlark.com>. I have a website at
http://cowlark.com. There may or may not be anything interesting there.

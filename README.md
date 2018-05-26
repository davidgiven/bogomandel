# Bogomandel

A less stupid mandelbrot program for the BBC Micro. v1.0, 2018-05-26


## What?

This started life as a really hacky toy which could draw a sort-of
Mandelbrotish image on a BBC Micro in about 12 seconds; it grew into a much
less hacky toy which can generate almost real-time (usually 30s per frame)
Mandelbrots and Julia sets with 14-bit fixed point images. It mostly achieves
these using a really clever Mandelbrot kernel written by reenigne, aka Andrew
Jenner, so don't blame me.


## How?

To build, you need beebasm. There's a single mandel.asm file in the src
directory; assemble this. There's a Makefile with the right command in it.

The resulting problem will only run on an BBC Master 128, as it uses one bank
of sideways RAM (#4).


## Why?

Why not?

This has been a really interesting learning experience in machine code, fixed
point arithmetic and cheating the system. There's too much to go into here, but
here's the writeup of my 8-bit fixed point version:

	http://cowlark.com/2018-05-21-bbc-micro-mandelbrot/

...and here's the writeup of the current, much better version:

<URL will appear later>


## Why not?

14 bits isn't really enough precision to do much, as you will notice once you
hit the zoom limit. (Try looking at Julia sets at maximum zoom. Lots of
artifacts there!)


## License?

Two-clause BSD; see the COPYING file. Go nuts.


## Who?

This was written by two people:

Most of the program was by myself, David Given <dg@cowlark.com>; I have a
website at http://cowlark.com. There may or may not be anything interesting
there.

The kernel is by reenigne, aka Andrew Jenner; reenigne's musings on very fast
Mandelbrots on the 8088 led to this program, so you might like to check out
http://github.com/reenigne/reenigne/tree/master/8088/mandel. (This was then
somewhat hacked by me, so bugs will be my fault.)


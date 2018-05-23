#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define FRACTION_BITS 9
#define INTEGER_BITS 5

#define TOTAL_BITS (FRACTION_BITS + INTEGER_BITS)

int main(int argc, const char* argv[])
{
    for (int i = 0; i < (1<<TOTAL_BITS); i+=2)
    {
        int16_t f = (int16_t)(i << (16-TOTAL_BITS)) >> (16-TOTAL_BITS);
        double d = ((double)f) / (1<<FRACTION_BITS);
        double sd = d * d;
        int sf = (int)(sd * (1<<FRACTION_BITS));
        sf &= 0x3ffe;
        sf |= 0x8000;

        putchar(sf);
        putchar(sf >> 8);
        // printf("%d^2 = %d = %f = %f = %x\n", i, f, d, sd, sf);
    }

    exit(0);
}
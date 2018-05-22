#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

int main(int argc, const char* argv[])
{
    for (int i = 0; i < 256; i++)
    {
        int8_t f = (int8_t) i;
        double d = ((double)f) / 64.0;
        double sd = d * d;
        int8_t sf = (int8_t)(sd * 64.0);
        if (sf < 0)
            sf = 0x80;
        putchar(sf);
    }

    exit(0);
}
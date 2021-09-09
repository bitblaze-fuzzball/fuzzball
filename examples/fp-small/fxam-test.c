#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

short fxam80(long double ld) {
    short sw;
    asm("fxam; fstsw %0" : "=m" (sw) : "t" (ld));
    return ((sw >> 8) & 0x7) | ((sw >> 11) & 0x8);
}

short fxam80_empty(long double ld) {
    short sw;
    asm("ffree %%st(0); fxam; fstsw %0" : "=m" (sw) : "t" (ld));
    return ((sw >> 8) & 0x7) | ((sw >> 11) & 0x8);
}

short fxam64(double d) {
    short sw;
    asm("fxam; fstsw %0" : "=m" (sw) : "t" (d));
    return ((sw >> 8) & 0x7) | ((sw >> 11) & 0x8);
}

short fxam64_empty(double d) {
    short sw;
    asm("ffree %%st(0); fxam; fstsw %0" : "=m" (sw) : "t" (d));
    return ((sw >> 8) & 0x7) | ((sw >> 11) & 0x8);
}

void test_fxam80(long double ld) {
    unsigned char *b = (unsigned char *)&ld;
    short kind = fxam80(ld);
    printf("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x   %x %Lg\n",
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0],
	   kind, ld);
}

void test_fxam80_empty(long double ld) {
    unsigned char *b = (unsigned char *)&ld;
    short kind = fxam80_empty(ld);
    printf("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x E %x %Lg\n",
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0],
	   kind, ld);
}

void test_fxam64(double d) {
    unsigned long long d_bits = *(unsigned long long *)&d;
    short kind = fxam64(d);
    printf("    %016llx   %x %g\n", d_bits, kind, d);
}

void test_fxam64_empty(double d) {
    unsigned long long d_bits = *(unsigned long long *)&d;
    short kind = fxam64_empty(d);
    printf("    %016llx E %x %g\n", d_bits, kind, d);
}

int main(int argc, char **argv) {
    test_fxam64(-(0.0/0.0));
    test_fxam64(0.0/0.0);
    test_fxam64(5.0);
    test_fxam64(1.0/0.0);
    test_fxam64(-1.0);
    test_fxam64(-1.0/0.0);
    test_fxam64(0);
    test_fxam64_empty(5.0);
    test_fxam64(-0.0);
    test_fxam64_empty(-1);

    test_fxam80(-(0.0/0.0));
    test_fxam80(0.0/0.0);
    test_fxam80(5.0);
    test_fxam80(1.0/0.0);
    test_fxam80(-1.0);
    test_fxam80(-1.0/0.0);
    test_fxam80(0);
    test_fxam80_empty(5.0);
    test_fxam80(-0.0);
    test_fxam80_empty(-1.0);
    test_fxam80(5e-4940L);
    test_fxam80(-3e-4947L);

    return 0;
}

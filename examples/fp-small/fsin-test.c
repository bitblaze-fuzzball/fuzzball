#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double fsin_insn(double d) {
    asm("fsin" : "+t" (d));
    return d;
}

int main(int argc, char **argv) {
    unsigned long long bits64;
    double d, sin_d_lib, sin_d_insn;
    if (argc != 2) {
	fprintf(stderr, "Usage: fsin-test <hex>\n");
	exit(1);
    }
    bits64 = strtoull(argv[1], 0, 0);
    memcpy(&d, &bits64, 8);
    printf("Input is %a\n", d);
    sin_d_lib = sin(d);
    printf("Sine value (library) is %a\n", sin_d_lib);
    sin_d_insn = fsin_insn(d);
    printf("Sine value (insn)    is %a\n", sin_d_insn);
    return 0;
}

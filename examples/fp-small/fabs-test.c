#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double fabs_insn(double d) {
    asm("fabs" : "+t" (d));
    return d;
}

int main(int argc, char **argv) {
    unsigned long long bits64;
    double d, abs_d_lib, abs_d_insn;
    if (argc != 2) {
	fprintf(stderr, "Usage: fabs-test <hex>\n");
	exit(1);
    }
    bits64 = strtoull(argv[1], 0, 0);
    memcpy(&d, &bits64, 8);
    printf("Input is %a\n", d);
    abs_d_lib = fabs(d);
    printf("Absolute value (library) is %a\n", abs_d_lib);
    abs_d_insn = fabs_insn(d);
    printf("Absolute value (insn) is %a\n", abs_d_insn);
    return 0;
}

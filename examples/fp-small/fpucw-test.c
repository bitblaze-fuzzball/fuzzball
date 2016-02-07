#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char **argv) {
    float f;
    double d;
    long double ld;
    if (argc != 2) {
	fprintf(stderr, "Usage: fpucw-test <float>\n");
	return 1;
    }
    f = strtof(argv[1], 0);
    d = strtod(argv[1], 0);
    ld = strtold(argv[1], 0);
    printf("32-bit %a: %snegative\n", f, (signbit(f) ? "" : "non-"));
    printf("64-bit %a: %snegative\n", d, (signbit(d) ? "" : "non-"));
    printf("80-bit %La: %snegative\n", ld, (signbit(ld) ? "" : "non-"));
    return 0;
}

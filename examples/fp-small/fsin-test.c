#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double fsin_insn(double d) {
    asm("fsin" : "+t" (d));
    return d;
}

/*   x    - x**2 *            3!  = 6
    (x/3! + x**2 *            5!  = 120
    (x/5! - x**2 *            7!  = 5040
    (x/7! + x**2 *            9!  = 362880
    (x/9! - x**2 *            11! = 39916800
    (x/11! + x**2 *           13! = 6227020800
    (x/13! - x**3/15!))))))   15! = 1307674368000 */

#define CONST_2PI     0x1.921fb54442d18p+1
#define CONST_1o3fac  0x1.5555555555555p-3
#define CONST_1o5fac  0x1.1111111111111p-7
#define CONST_1o7fac  0x1.a01a01a01a01ap-13
#define CONST_1o9fac  0x1.71de3a556c734p-19
#define CONST_1o11fac 0x1.ae64567f544e4p-26
#define CONST_1o13fac 0x1.6124613a86d09p-33
#define CONST_1o15fac 0x1.ae7f3e733b81fp-41

double sin_approx(double x_unred) {
    double x = remainder(x_unred, CONST_2PI);
    double x2 = x*x;
    double term15 = x*CONST_1o15fac;
    double term13 = x*CONST_1o13fac - x2*term15;
    double term11 = x*CONST_1o11fac + x2*term13;
    double term9  = x*CONST_1o9fac  - x2*term11;
    double term7  = x*CONST_1o7fac  + x2*term9;
    double term5  = x*CONST_1o5fac  - x2*term7;
    double term3  = x*CONST_1o3fac  + x2*term5;
    double tot = x - x2 * term3;
    return tot;
}

int main(int argc, char **argv) {
    unsigned long long bits64;
    double d, sin_d_lib, sin_d_insn, sin_d_approx;
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
    sin_d_approx = sin_approx(d);
    printf("Sine value (approx)  is %a\n", sin_d_approx);
    return 0;
}

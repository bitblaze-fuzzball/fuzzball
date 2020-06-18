#include <math.h>
#include <stdio.h>

typedef float float_x4 __attribute__ ((vector_size (16)));

float maxf_sse(float a, float b) {
    float_x4 a4 = {a, 0, 0, 0};
    float_x4 b4 = {b, 0, 0, 0};
    asm("maxss %1, %0" : "+x" (a4) : "x" (b4));
    return a4[0];
}

float maxf_choice(float a, float b) {
    if (a <= b)
	return b;
    else if (a != a || b != b)
	return b;
    else
	return a;
}

void test_maxf(float a, float b) {
    printf("max(%.2f, %.2f) = %.2f\n", a, b, maxf_sse(a, b));
}

int main(int argc, char **argv) {
    float nan = nanf("");
    float inf = 1/0.0;
    test_maxf(1.0, 2.0);
    test_maxf(2.0, 1.0);
    test_maxf(+0.0, -0.0);
    test_maxf(-0.0, +0.0);
    test_maxf(nan, 1);
    test_maxf(1, nan);
    test_maxf(nan, nan);
    test_maxf(inf, -inf);
    return 0;
}

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

union f2bu { float f; unsigned int b; };

static inline unsigned int f2b(float f) {
    union f2bu u;
    u.f = f;
    return u.b;
}

static inline float b2f(unsigned int b) {
    union f2bu u;
    u.b = b;
    return u.f;
}


float sqrt_f32_a1n(float x, int iters) {
    unsigned int xi = f2b(x);
    float denorm = 1, r;
    int i;
    if (xi < 0x00800000) {
	x *= 0x1p32;
	denorm = 0x1p-16;
	xi = f2b(x);
    }
    xi = (1 << 29) + (xi >> 1) - (1 << 22) - 307410;
    r = b2f(xi);
    for (i = 0; i < iters; i++) {
	r = (r + x / r) / 2;
    }
    return denorm * r;
}

void f32_test_loop(float (*fp)(float, int), int iters, long long n) {
    long long i;
    int worst_bits = 0;
    float worst_float = 0;
    double worst_err = 0;
    for (i = 0; i < n; i++) {
	int ri = rand();
	float rf, a_sqrt, exact;
	double err;
	ri &= 0x7fffffff;
	/* if (ri < 0x00800000 || ri > 0x7f7fffff)
	    continue; */
	rf = b2f(ri);
	a_sqrt = (*fp)(rf, iters);
	exact = sqrtf(rf);
	err = fabs((double)exact - a_sqrt)/exact;
	if (err > worst_err) {
	    worst_err = err;
	    worst_bits = ri;
	    worst_float = rf;
	}
    }
    printf("Worst-case error is %.10f%%, at %g (0x%08x)\n",
	   worst_err * 100, (double)worst_float, worst_bits);
}

union d2bu { double d; unsigned long long b; };

static inline unsigned long long d2b(double d) {
    union d2bu u;
    u.d = d;
    return u.b;
}

static inline double b2d(unsigned long long b) {
    union d2bu u;
    u.b = b;
    return u.d;
}

double sqrt_f64_a1n(double x, int iters) {
    unsigned long long xi = d2b(x);
    double denorm = 1.0, r;
    int i;
    if (xi < 0x0010000000000000) {
	x *= 0x1p64;
	denorm = 0x1p-32;
	xi = d2b(x);
    }
    xi = (1LL << 61) + (xi >> 1) - (1LL << 51) - 165039487057920.0;
    r = b2d(xi);
    for (i = 0; i < iters; i++) {
	r = (r + x / r) / 2;
    }
    return denorm * r;
}

void f64_test_loop(double (*fp)(double, int), int iters, long long n) {
    long long i;
    long long worst_bits = 0;
    double worst_float = 0;
    long double worst_err = 0;
    for (i = 0; i < n; i++) {
	unsigned long long ri;
	double rf, a_sqrt, exact;
	long double err;
	ri = (rand() & 0x00ffffffLL);
	ri |= (rand() & 0x00ffffffLL) << 24;
	ri |= (rand() & 0x00007fffLL) << 48;
	rf = b2d(ri);
	a_sqrt = (*fp)(rf, iters);
	exact = sqrt(rf);
	err = fabsl((long double)exact - a_sqrt)/exact;
	if (err > worst_err) {
	    worst_err = err;
	    worst_bits = ri;
	    worst_float = rf;
	}
    }
    printf("Worst-case error is %.17Lf%%, at %g (0x%016llx)\n",
	   worst_err * 100.0, worst_float, worst_bits);
}

int main(int argc, char **argv) {
    srand(1);
    assert(RAND_MAX == 2147483647);

    printf("Single precision:\n");
    printf("sqrt(2) is about %.7g\n", sqrt_f32_a1n(2.0, 3));
    f32_test_loop(&sqrt_f32_a1n, 0, 200000000);
    f32_test_loop(&sqrt_f32_a1n, 1, 200000000);
    f32_test_loop(&sqrt_f32_a1n, 2, 200000000);
    f32_test_loop(&sqrt_f32_a1n, 3, 200000000);
    f32_test_loop(&sqrt_f32_a1n, 4, 200000000);

    printf("Double precision:\n");
    printf("sqrt(2) is about %.15g\n", sqrt_f64_a1n(2.0, 4));
    f64_test_loop(&sqrt_f64_a1n, 0, 100000000);
    f64_test_loop(&sqrt_f64_a1n, 1, 100000000);
    f64_test_loop(&sqrt_f64_a1n, 2, 100000000);
    f64_test_loop(&sqrt_f64_a1n, 3, 100000000);
    f64_test_loop(&sqrt_f64_a1n, 4, 100000000);
    f64_test_loop(&sqrt_f64_a1n, 5, 100000000);
    return 0;
}

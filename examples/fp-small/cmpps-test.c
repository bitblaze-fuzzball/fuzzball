#include <stdio.h>
#include <string.h>
#include <math.h>

typedef float float_x4 __attribute__ ((vector_size (16)));

typedef int int_x4 __attribute__ ((vector_size (16)));

int_x4 compare_float_x4_c(float_x4 a, float_x4 b, int op) {
    int_x4 result = {0, 0, 0, 0};
    switch (op) {
    case 0: result = (a == b);  break;
    case 1: result = (a < b);   break;
    case 2: result = (a <= b);  break;
    case 4: result = (a != b);  break;
    case 5: result = ~(a < b);  break;
    case 6: result = ~(a <= b); break;
    }
    return result;
}

int_x4 compare_float_x4_asm(float_x4 a, float_x4 b, int op) {
    int_x4 result = {0, 0, 0, 0};
    memcpy(&result, &a, 16);
    switch (op) {
    case 0: asm("cmpeqps    %1, %0" : "=x" (result) : "x" (b)); break;
    case 1: asm("cmpltps    %1, %0" : "=x" (result) : "x" (b)); break;
    case 2: asm("cmpleps    %1, %0" : "=x" (result) : "x" (b)); break;
    case 3: asm("cmpunordps %1, %0" : "=x" (result) : "x" (b)); break;
    case 4: asm("cmpneqps   %1, %0" : "=x" (result) : "x" (b)); break;
    case 5: asm("cmpnltps   %1, %0" : "=x" (result) : "x" (b)); break;
    case 6: asm("cmpnleps   %1, %0" : "=x" (result) : "x" (b)); break;
    case 7: asm("cmpordps   %1, %0" : "=x" (result) : "x" (b)); break;
    }
    return result;
}

int main(int argc, char **argv) {
    float_x4 a = {-1, -0, 0, 1};
    float_x4 b = {1, 0, -0, nanf("")};
    int_x4 res;
    int i;
    for (i = 0; i <= 7; i++) {
	res = compare_float_x4_asm(a, b, i);
	printf("%08x %08x %08x %08x\n", res[0], res[1], res[2], res[3]);
    }
    return 0;
}

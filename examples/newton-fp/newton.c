
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

union ieee754_double {
    double d;

    /* This is the IEEE 754 double-precision format.  */
    struct {
        unsigned int mantissa1:32;
        unsigned int mantissa0:20;
        unsigned int exponent:11;
        unsigned int negative:1;
    } ieee;
};

/* This is a simplified version of glibc's
   sysdeps/generic/printf_fphex.c:__printf_fphex */
int fphex(double d, char *outbuf) {
    union ieee754_double fpnum;
    char numbuf[32];
    char *numend = &numbuf[sizeof numbuf];
    char *numstr = numend;
    char expbuf[5];
    char *expend = &expbuf[sizeof expbuf];
    char *expstr = expend;
    int expnegative;
    int precision;
    fpnum.d = d;
    int negative = fpnum.ieee.negative;
    unsigned long long num =
	(unsigned long long)fpnum.ieee.mantissa0 << 32 | fpnum.ieee.mantissa1;
    int zero_mantissa = (num == 0);
    while (num) {
	int digit = num & 0xf;
	*--numstr = (digit < 10 ? '0' + digit : 'a' + digit - 10);
	num >>= 4;
    }
    if (numstr == numend)
	*--numstr = '0';
    while (numstr > numbuf + (sizeof numbuf - 13)) {
	*--numstr = '0';
    }
    int exponent = fpnum.ieee.exponent;
    char leading = (exponent == 0 ? '0' : '1');
    if (exponent == 0) {
	if (zero_mantissa)
	    expnegative = 0;
	else {
	    // Denormalized
	    expnegative = 1;
	    exponent = 0x3ff - 1;
	}
    } else if (exponent >= 0x3ff) {
	expnegative = 0;
	exponent -= 0x3ff;
    } else {
	expnegative = 1;
	exponent = 0x3ff - exponent;
    }
    if (zero_mantissa) {
	precision = 0;
	numend = numstr;
    } else {
	while (numend[-1] == '0')
	    --numend;
	assert(numend >= numbuf);
	precision = numend - numstr;
    }
    while (exponent) {
	int digit = exponent % 10;
	*--expstr = '0' + digit;
	exponent /= 10;
    }
    if (expstr == expend)
	*--expstr = '0';
    char *outp = outbuf, *p;
    if (negative)
	*outp++ = '-';
    *outp++ = '0';
    *outp++ = 'x';
    *outp++ = leading;
    if (precision > 0) {
	*outp++ = '.';
	for (p = numstr; p < numend; p++)
	    *outp++ = *p;
    }
    *outp++ = 'p';
    *outp++ = (expnegative ? '-' : '+');
    for (p = expstr; p < expend; p++)
	*outp++ = *p;
    *outp++ = '\0';
    return outp - outbuf;
}

const char space = ' ';
const char newline = '\n';

double newton_sqrt(double a, double initial) {
    double x = initial;
    int count = 20;
    char outbuf[40];
    int len;
    while (count > 0) {
	double old_x = x;
	x = old_x - (x * x - a)/(2 * x);
	double error = x - old_x;
	if (error < 0)
	    error = -error;
	if (error == 0.0) {
	    count = 0;
	} else if (count > 1 && error / x < 1e-15) {
	    count = 1;
	}
	len = fphex(x, outbuf); write(1, outbuf, len - 1);
	write(1, &space, 1);
	len = fphex(error, outbuf); write(1, outbuf, len - 1);
	write(1, &newline, 1);
	count--;
    }
    return x;
}

int main(int argc, char **argv) {
    int n;
    for (n = 2; n <= 10; n++) {
	double a = (double)n;
	double s = newton_sqrt(a, 1.5);
	(void)s;
	write(1, &newline, 1);
    }
    return 0;
}

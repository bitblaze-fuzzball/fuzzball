#include <math.h>
#include <stdio.h>
#include <stdint.h>

/* Soft implementation for comparison from Sun/glibc: */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

/* A union which permits us to convert between a float and a 32 bit
   int.  */

typedef union
{
  float value;
  uint32_t word;
} ieee_float_shape_type;

/* Get a 32 bit int from a float.  */
#ifndef GET_FLOAT_WORD
# define GET_FLOAT_WORD(i,d)                                    \
do {                                                            \
  ieee_float_shape_type gf_u;                                   \
  gf_u.value = (d);                                             \
  (i) = gf_u.word;                                              \
} while (0)
#endif

/* Set a float from a 32 bit int.  */
#ifndef SET_FLOAT_WORD
# define SET_FLOAT_WORD(d,i)                                    \
do {                                                            \
  ieee_float_shape_type sf_u;                                   \
  sf_u.word = (i);                                              \
  (d) = sf_u.value;                                             \
} while (0)
#endif

static  const float     one     = 1.0, tiny=1.0e-30;

float
__ieee754_sqrtf(float x)
{
        float z;
        int32_t sign = (int)0x80000000;
        int32_t ix,s,q,m,t,i;
        uint32_t r;

        GET_FLOAT_WORD(ix,x);

    /* take care of Inf and NaN */
        if((ix&0x7f800000)==0x7f800000) {
            return x*x+x;               /* sqrt(NaN)=NaN, sqrt(+inf)=+inf
                                           sqrt(-inf)=sNaN */
        }
    /* take care of zero */
        if(ix<=0) {
            if((ix&(~sign))==0) return x;/* sqrt(+-0) = +-0 */
            else if(ix<0)
                return (x-x)/(x-x);             /* sqrt(-ve) = sNaN */
        }
    /* normalize x */
        m = (ix>>23);
        if(m==0) {                              /* subnormal x */
            for(i=0;(ix&0x00800000)==0;i++) ix<<=1;
            m -= i-1;
        }
        m -= 127;       /* unbias exponent */
        ix = (ix&0x007fffff)|0x00800000;
        if(m&1) /* odd m, double x to make it even */
            ix += ix;
        m >>= 1;        /* m = [m/2] */

    /* generate sqrt(x) bit by bit */
        ix += ix;
        q = s = 0;              /* q = sqrt(x) */
        r = 0x01000000;         /* r = moving bit from right to left */

        while(r!=0) {
            t = s+r;
            if(t<=ix) {
                s    = t+r;
                ix  -= t;
                q   += r;
            }
            ix += ix;
            r>>=1;
        }


    /* use floating add to find out rounding direction */
        if(ix!=0) {
            z = one-tiny; /* trigger inexact flag */
            if (z>=one) {
                z = one+tiny;
                if (z>one)
                    q += 2;
                else
                    q += (q&1);
            }
        }
        ix = (q>>1)+0x3f000000;
        ix += (m <<23);
        SET_FLOAT_WORD(z,ix);
        return z;
}

/* end of Sun/glibc code */

typedef float float_x4 __attribute__ ((vector_size (16)));

float fsqrt_f32(float x) {
    float result;
    asm("flds %1; fsqrt; fstps %0" : "=m" (result) : "m" (x));
    return result;
}

float sqrtss_f32(float x) {
    float_x4 result;
    float_x4 in = {x, 0, 0, 0};
    asm("sqrtss %1, %0" : "=x" (result) : "x" (in));
    return result[0];
}

void test_sqrt_f32(float x) {
    float r1 = fsqrt_f32(x);
    float r2 = sqrtss_f32(x);
    float goal = __ieee754_sqrtf(x);
    printf("%08x  %08x %08x %08x (%g)\n", *(unsigned int *)&x,
	   *(unsigned int *)&goal,
	   *(unsigned int *)&r1, *(unsigned int *)&r2, x);
}

void tests_f32(void) {
    test_sqrt_f32(1e33);
    test_sqrt_f32(1e10);
    test_sqrt_f32(65536.0);
    test_sqrt_f32(4.0);
    test_sqrt_f32(3.0);
    test_sqrt_f32(2.0);
    test_sqrt_f32(1.0);
    test_sqrt_f32(1e-44);
    test_sqrt_f32(0.0);
    test_sqrt_f32(-0.0);
    test_sqrt_f32(-1.0);
    test_sqrt_f32(1.0/0.0);
    test_sqrt_f32(-1.0/0.0);
}

typedef double double_x2 __attribute__ ((vector_size (16)));

double fsqrt_f64(double x) {
    double result;
    asm("fldl %1; fsqrt; fstpl %0" : "=m" (result) : "m" (x));
    return result;
}

double sqrtsd_f64(double x) {
    double_x2 result;
    double_x2 in = {x, 0};
    asm("sqrtsd %1, %0" : "=x" (result) : "x" (in));
    return result[0];
}

void test_sqrt_f64(double x) {
    double r1 = fsqrt_f64(x);
    double r2 = sqrtsd_f64(x);
    printf("%016llx  %016llx %016llx (%g)\n", *(unsigned long long *)&x,
	   *(unsigned long long *)&r1, *(unsigned long long *)&r2, x);
}

void tests_f64(void) {
    test_sqrt_f64(1e300);
    test_sqrt_f64(1e10);
    test_sqrt_f64(65536.0);
    test_sqrt_f64(4.0);
    test_sqrt_f64(3.0);
    test_sqrt_f64(2.0);
    test_sqrt_f64(1.0);
    test_sqrt_f64(1e-322);
    test_sqrt_f64(0.0);
    test_sqrt_f64(-0.0);
    test_sqrt_f64(-1.0);
    test_sqrt_f64(1.0/0.0);
    test_sqrt_f64(-1.0/0.0);
}

int main(int argc, char **argv) {
    tests_f32();
    tests_f64();
    return 0;
}

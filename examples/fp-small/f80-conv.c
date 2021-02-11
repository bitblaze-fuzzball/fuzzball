#include <assert.h>
#include <stdio.h>
#include <string.h>

/* This function is written in a style similar to our Vine IR
   implementation, and so can be used to debug its algorithm. */
double loadf80_tof64(long double *p) {
    unsigned long long frac = *(unsigned long long *)p;
    unsigned short sexp = *(unsigned short *)((char *)p + 8);
    unsigned long long ef64;
    unsigned long long f64;
    int exp80 = sexp & 0x7fff;
    int sign = (sexp & 0x8000) != 0;
    unsigned long long sign64 =
	sign ? 0x8000000000000000LL : 0x000000000000000LL;
    if (exp80 == 0) {
	/* F80 +/- zero or denorm converts to F64 +/- zero */
	ef64 = 0x000000000000000LL;
    } else if (exp80 == 0x7fff) {
	if (frac == 0x8000000000000000LL) {
	    /* +/- infinity */
	    ef64 = 0x7ff0000000000000LL;
	} else {
	    /* NaN */
	    ef64 = 0x7ff8000000000000LL;
	}
    } else {
	/* adjust biased exponent to new bias: -(16383-1023) = 0xffffc400 */
	int new_exp = exp80 - (16383 - 1023);
	if (new_exp <= -52) {
	    /* underflow to 0 */
	    ef64 = 0x000000000000000LL;
	} else if (new_exp <= 0) {
	    /* denorm in F64 */
	    ef64 = frac >> (12 - new_exp);
	} else if (new_exp >= 0x7ff) {
	    /* overflow to infinity */
	    ef64 = 0x7ff0000000000000LL;
	} else {
	    /* Put the exponent in its correct position */
	    unsigned long long exp64 = (unsigned long long)new_exp << 52;
	    /* Remove explicit "1." bit, shift the rest over */
	    unsigned long long frac64 = (frac & 0x7fffffffffffffffLL) >> 11;
	    ef64 = exp64 | frac64;
	}
    }
    f64 = ef64 | sign64;
    return *(double *)&f64;
}

void test_lconv(long double x) {
    unsigned char *b = (unsigned char *)&x;
    double d = (double)x;
    double d2 = loadf80_tof64(&x);
#if 1
    printf("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x %016llx %016llx %Lg\n",
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0],
	   *(unsigned long long *)&d, *(unsigned long long *)&d2, x);
#else
    printf("    test_conv_bits(0x%02x%02x, "
	   "0x%02x%02x%02x%02x%02x%02x%02x%02xLL);\n",
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0]);
#endif
}

void test_lconv_bits(unsigned short sexp_bits, unsigned long long frac_bits) {
    long double x;
    unsigned char *b = (unsigned char *)&x;
    double d, d2;
    memcpy(b, &frac_bits, 8);
    memcpy(b + 8, &sexp_bits, 2);
    d = (double)x;
    d2 = loadf80_tof64(&x);
    printf("%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x %016llx %016llx %Lg\n",
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0],
	   *(unsigned long long *)&d, *(unsigned long long *)&d2, x);
}

/* This version of the code generates the test data by doing its own
   long double operations. This is generally more convenient, but it
   has the problems of not creating the right tests if you can't
   properly do long double operations, like Valgrind and FuzzBALL. */
void test_lconv_from_comp(void) {
    int i;
    long double x = 3.2L;
    for (i = 0; i < 80; i++) {
	test_lconv(x);
	x *= 17821.984;
    }
    puts("");
    x = 0.32L;
    for (i = 0; i < 80; i++) {
	test_lconv(x);
	x /= 17821.984;
    }
    puts("");
    test_lconv(0.0L);
    test_lconv(-0.0L);
    test_lconv(1.0/0.0);
    test_lconv(-1.0/0.0);
    test_lconv(0.0/0.0);
    test_lconv(-3);
    test_lconv(-1/3.0L);
}

/* The same tests as test_from_comp, but with the F80 bits specified
   directly in hex integers. */
void test_lconv_from_bits(void) {
    test_lconv_bits(0x4000, 0xcccccccccccccccdLL);
    test_lconv_bits(0x400e, 0xdec6594af4f0d99aLL);
    test_lconv_bits(0x401c, 0xf253c3af4fe446c8LL);
    test_lconv_bits(0x402b, 0x83cc3f20e87c4e01LL);
    test_lconv_bits(0x4039, 0x8f5d8bdbf3b0b5c1LL);
    test_lconv_bits(0x4047, 0x9bf2c08d83999291LL);
    test_lconv_bits(0x4055, 0xa9a2ace8e0df6380LL);
    test_lconv_bits(0x4063, 0xb886212aa2b58b13LL);
    test_lconv_bits(0x4071, 0xc8b81b149b29aaa3LL);
    test_lconv_bits(0x407f, 0xda55f6dc7cf78fe7LL);
    test_lconv_bits(0x408d, 0xed7fa465f0595fa4LL);
    test_lconv_bits(0x409c, 0x812bf0944ab9a9f3LL);
    test_lconv_bits(0x40aa, 0x8c823b957588a1a4LL);
    test_lconv_bits(0x40b8, 0x98d740c223665e89LL);
    test_lconv_bits(0x40c6, 0xa6415b71088827c6LL);
    test_lconv_bits(0x40d4, 0xb4d8dd4baef9233bLL);
    test_lconv_bits(0x40e2, 0xc4b83a64eba8e121LL);
    test_lconv_bits(0x40f0, 0xd5fc392de93db12aLL);
    test_lconv_bits(0x40fe, 0xe8c426a0b9a06271LL);
    test_lconv_bits(0x410c, 0xfd320eff00a78e36LL);
    test_lconv_bits(0x411b, 0x89b57dc5cb95f32fLL);
    test_lconv_bits(0x4129, 0x95cb9ad78698b31eLL);
    test_lconv_bits(0x4137, 0xa2f1480062088025LL);
    test_lconv_bits(0x4145, 0xb13e5adc843cc387LL);
    test_lconv_bits(0x4153, 0xc0ccc08e19899b7dLL);
    test_lconv_bits(0x4161, 0xd1b8acbdbd505cbcLL);
    test_lconv_bits(0x416f, 0xe420ccbaf146abd8LL);
    test_lconv_bits(0x417d, 0xf8267f1952eff2bdLL);
    test_lconv_bits(0x418c, 0x86f70817b0e89460LL);
    test_lconv_bits(0x419a, 0x92cf7df2411d676aLL);
    test_lconv_bits(0x41a8, 0x9fb21aa2c455b88dLL);
    test_lconv_bits(0x41b6, 0xadb63a30c91c982fLL);
    test_lconv_bits(0x41c4, 0xbcf5457e290f0d94LL);
    test_lconv_bits(0x41d2, 0xcd8ae057b00ba2b1LL);
    test_lconv_bits(0x41e0, 0xdf951b90c69ba976LL);
    test_lconv_bits(0x41ee, 0xf334ab84f6c475f1LL);
    test_lconv_bits(0x41fd, 0x844691b10dbde786LL);
    test_lconv_bits(0x420b, 0x8fe29ad35b959c15LL);
    test_lconv_bits(0x4219, 0x9c837d24ae5d33a5LL);
    test_lconv_bits(0x4227, 0xaa401d842cce3b99LL);
    test_lconv_bits(0x4235, 0xb9316335fc77d4e9LL);
    test_lconv_bits(0x4243, 0xc9726508ee10abf6LL);
    test_lconv_bits(0x4251, 0xdb209a728d5d112cLL);
    test_lconv_bits(0x425f, 0xee5c10fa9c75aef4LL);
    test_lconv_bits(0x426e, 0x81a3d32b694e70acLL);
    test_lconv_bits(0x427c, 0x8d04a3d01b61e57cLL);
    test_lconv_bits(0x428a, 0x99651b0a5666d66fLL);
    test_lconv_bits(0x4298, 0xa6dba8f0ada55fd4LL);
    test_lconv_bits(0x42a6, 0xb580b5bec003b629LL);
    test_lconv_bits(0x42b4, 0xc56ece14981f4620LL);
    test_lconv_bits(0x42c2, 0xd6c2d31839881572LL);
    test_lconv_bits(0x42d0, 0xe99c2ed09cc4dd8fLL);
    test_lconv_bits(0x42de, 0xfe1d0d19044bd803LL);
    test_lconv_bits(0x42ed, 0x8a354cc9f499d6a4LL);
    test_lconv_bits(0x42fb, 0x9656a186e711c936LL);
    test_lconv_bits(0x4309, 0xa388826511336e2aLL);
    test_lconv_bits(0x4317, 0xb1e2db1f8beedf76LL);
    test_lconv_bits(0x4325, 0xc17fb0e87c21550bLL);
    test_lconv_bits(0x4333, 0xd27b51951b0348dbLL);
    test_lconv_bits(0x4341, 0xe4f486eda0ef27ddLL);
    test_lconv_bits(0x434f, 0xf90cce7e1882708dLL);
    test_lconv_bits(0x435e, 0x87744b26a51a12bcLL);
    test_lconv_bits(0x436c, 0x9357bf73e902ee77LL);
    test_lconv_bits(0x437a, 0xa046519b8d0512b2LL);
    test_lconv_bits(0x4388, 0xae5773533b4f6fd5LL);
    test_lconv_bits(0x4396, 0xbda4a51207bcc23dLL);
    test_lconv_bits(0x43a4, 0xce49a44bd7db8186LL);
    test_lconv_bits(0x43b2, 0xe0649dbb8ec9cacdLL);
    test_lconv_bits(0x43c0, 0xf41664172862cdbfLL);
    test_lconv_bits(0x43cf, 0x84c155c877d74d86LL);
    test_lconv_bits(0x43dd, 0x90682548d8611306LL);
    test_lconv_bits(0x43eb, 0x9d14c0109f05b4b9LL);
    test_lconv_bits(0x43f9, 0xaade203e76c78092LL);
    test_lconv_bits(0x4407, 0xb9dd4433734679b3LL);
    test_lconv_bits(0x4415, 0xca2d5be2a3d48b06LL);
    test_lconv_bits(0x4423, 0xdbebfa1ab7940837LL);
    test_lconv_bits(0x4431, 0xef394a230d0e34a2LL);
    test_lconv_bits(0x4440, 0x821c2506afa8d082LL);
    test_lconv_bits(0x444e, 0x8d878512e53f65d1LL);
    test_lconv_bits(0x445c, 0x99f378fa1495cdd8LL);
    puts("");
    test_lconv_bits(0x3ffd, 0xa3d70a3d70a3d70aLL);
    test_lconv_bits(0x3fef, 0x969ed346246c93a0LL);
    test_lconv_bits(0x3fe1, 0x8a77ab517c2b67d2LL);
    test_lconv_bits(0x3fd2, 0xfe97145caabb793aLL);
    test_lconv_bits(0x3fc4, 0xea0c5d82491b5c94LL);
    test_lconv_bits(0x3fb6, 0xd729f497d4702c4bLL);
    test_lconv_bits(0x3fa8, 0xc5cd9d59369ae7c4LL);
    test_lconv_bits(0x3f9a, 0xb5d7dea9960d0c69LL);
    test_lconv_bits(0x3f8c, 0xa72bc984a7a3bec6LL);
    test_lconv_bits(0x3f7e, 0x99aec48a8e5d1b48LL);
    test_lconv_bits(0x3f70, 0x8d485bc731276febLL);
    test_lconv_bits(0x3f62, 0x81e2145d9b3d1641LL);
    test_lconv_bits(0x3f53, 0xeece878e126dc3d6LL);
    test_lconv_bits(0x3f45, 0xdb89d4b78208588eLL);
    test_lconv_bits(0x3f37, 0xc9d321c39217c5f5LL);
    test_lconv_bits(0x3f29, 0xb98a51c61b58849fLL);
    test_lconv_bits(0x3f1b, 0xaa91df231c1ce456LL);
    test_lconv_bits(0x3f0d, 0x9ccea60995e4836dLL);
    test_lconv_bits(0x3eff, 0x9027b33fe8347da1LL);
    test_lconv_bits(0x3ef1, 0x848616e87622f01eLL);
    test_lconv_bits(0x3ee2, 0xf3a975d92cebf823LL);
    test_lconv_bits(0x3ed4, 0xe0007984d7f70ab6LL);
    test_lconv_bits(0x3ec6, 0xcded9490b1b44da7LL);
    test_lconv_bits(0x3eb8, 0xbd5002ecae282e52LL);
    test_lconv_bits(0x3eaa, 0xae09a55548da3487LL);
    test_lconv_bits(0x3e9c, 0x9ffecab7d35da417LL);
    test_lconv_bits(0x3e8e, 0x9315fdfebae80cbfLL);
    test_lconv_bits(0x3e80, 0x8737d7eac2282193LL);
    test_lconv_bits(0x3e71, 0xf89da94b08e4403eLL);
    test_lconv_bits(0x3e63, 0xe48e5980af179844LL);
    test_lconv_bits(0x3e55, 0xd21d62b1ac4b38b1LL);
    test_lconv_bits(0x3e47, 0xc12956442342d36eLL);
    test_lconv_bits(0x3e39, 0xb193782d51014229LL);
    test_lconv_bits(0x3e2b, 0xa33f8739a26daec6LL);
    test_lconv_bits(0x3e1d, 0x961389d3afcf02b5LL);
    test_lconv_bits(0x3e0f, 0x89f79eed419f61cbLL);
    test_lconv_bits(0x3e00, 0xfdaba56a00a97b85LL);
    test_lconv_bits(0x3df2, 0xe933ed94a826605aLL);
    test_lconv_bits(0x3de4, 0xd662fb4e9c9d0238LL);
    test_lconv_bits(0x3dd6, 0xc516b1fc9349c57cLL);
    test_lconv_bits(0x3dc8, 0xb52fb59c8c864f11LL);
    test_lconv_bits(0x3dba, 0xa69131ebe93c8cc1LL);
    test_lconv_bits(0x3dac, 0x9920a623cc545a3eLL);
    test_lconv_bits(0x3d9e, 0x8cc5b4ed084b8278LL);
    test_lconv_bits(0x3d90, 0x8169f8347e426bf4LL);
    test_lconv_bits(0x3d81, 0xedf1b11fb5f24755LL);
    test_lconv_bits(0x3d73, 0xdabecfd2287ce443LL);
    test_lconv_bits(0x3d65, 0xc9187e59f3c78764LL);
    test_lconv_bits(0x3d57, 0xb8debd7d46531d4bLL);
    test_lconv_bits(0x3d49, 0xa9f422ed0d5d78c9LL);
    test_lconv_bits(0x3d3b, 0x9c3da3f150a851a4LL);
    test_lconv_bits(0x3d2d, 0x8fa264630d329989LL);
    test_lconv_bits(0x3d1f, 0x840b899aaed31fa3LL);
    test_lconv_bits(0x3d10, 0xf2c82202e8d277fbLL);
    test_lconv_bits(0x3d02, 0xdf3153f543fdfb6cLL);
    test_lconv_bits(0x3cf4, 0xcd2f25bee7674419LL);
    test_lconv_bits(0x3ce6, 0xbca0f19cad233381LL);
    test_lconv_bits(0x3cd8, 0xad68b4261755a047LL);
    test_lconv_bits(0x3cca, 0x9f6ad5e41e82b7ddLL);
    test_lconv_bits(0x3cbc, 0x928df94be3d71296LL);
    test_lconv_bits(0x3cae, 0x86baccc298c27f9fLL);
    test_lconv_bits(0x3c9f, 0xf7b7c0ae7cb41cdaLL);
    test_lconv_bits(0x3c91, 0xe3bafdcb31d8da8cLL);
    test_lconv_bits(0x3c83, 0xd15b14b7c6742980LL);
    test_lconv_bits(0x3c75, 0xc076b5c4f81d81e7LL);
    test_lconv_bits(0x3c67, 0xb0ef415405b36daeLL);
    test_lconv_bits(0x3c59, 0xa2a890524b9f668eLL);
    test_lconv_bits(0x3c4b, 0x9588c12f9da3b6acLL);
    test_lconv_bits(0x3c3d, 0x897808f2d794d8bfLL);
    test_lconv_bits(0x3c2e, 0xfcc1102f2b1aac25LL);
    test_lconv_bits(0x3c20, 0xe85c45cdc248317dLL);
    test_lconv_bits(0x3c12, 0xd59cba05e0c59522LL);
    test_lconv_bits(0x3c04, 0xc4606fc7c038b287LL);
    test_lconv_bits(0x3bf6, 0xb488281150ae0c87LL);
    test_lconv_bits(0x3be8, 0xa5f72948e0d87fc6LL);
    test_lconv_bits(0x3bda, 0x98930b29d45be460LL);
    test_lconv_bits(0x3bcc, 0x8c4386e4fff10a9aLL);
    test_lconv_bits(0x3bbe, 0x80f24b1de13a3664LL);
    test_lconv_bits(0x3baf, 0xed15a6e9d1a1a4b3LL);
    test_lconv_bits(0x3ba1, 0xd9f486aafa42f4ccLL);
    puts("");
    test_lconv_bits(0x0000, 0x0000000000000000LL);
    test_lconv_bits(0x8000, 0x0000000000000000LL);
    test_lconv_bits(0x7fff, 0x8000000000000000LL);
    test_lconv_bits(0xffff, 0x8000000000000000LL);
    test_lconv_bits(0xffff, 0xc000000000000000LL);
    test_lconv_bits(0xc000, 0xc000000000000000LL);
    test_lconv_bits(0xbffd, 0xaaaaaaaaaaaaaaabLL);
}

void storef64_tof80(double x, long double *out) {
    unsigned long long xb = *(unsigned long long *)&x;
    unsigned long long ef = xb & 0x7fffffffffffffff;
    int adjust_exp = 0;
    if (ef > 0 && ef < 0x0010000000000000) {
	/* To reduce the complexity of the special case for
	   denormalized numbers, handle them by temporarily increasing
	   their magnitude and then making a matching decrease in the
	   exponent later. */
	/* 2**64 is 0x43f0000000000000 */
	double x_norm = x * 0x1.0p64;
	xb = *(unsigned long long *)&x_norm;
	adjust_exp = 64;
    }
    unsigned long long frac = xb & 0x000fffffffffffff;
    int top12 = xb >> 52;
    int exp64 = top12 & 0x7ff;
    int sign = (top12 & 0x800) ? 1 : 0;
    int new_sign = sign ? 0x8000 : 0x0000;
    int new_exp;
    unsigned long long new_frac;
    if (exp64 == 0) {
	assert(frac == 0);
	/* +/- 0 */
	new_frac = 0;
	new_exp = 0;
    } else if (exp64 == 0x7ff) {
	new_exp = 0x7fff;
	if (frac == 0) {
	    /* +/- inf */
	    new_frac = 0x8000000000000000;
	} else {
	    /* NaN */
	    new_frac = 0xc000000000000000;
	}
    } else {
	/* Convert the exponent part by adding the difference between
	   the biases. 16383 - 1023 is 0x3c00 */
	new_exp = exp64 + (16383 - 1023);
	/* Correct for the normalization we might have done before */
	new_exp -= adjust_exp;
	/* The 52-bit fraction of the F64 turns into a 63-bit fraction
	   in F80 with an explicit leading 1. */
	new_frac = frac << 11;
	new_frac |= 0x8000000000000000;
    }
    *(unsigned long long *)out = new_frac;
    *((unsigned short *)((char *)out + 8)) = new_sign | new_exp;
}

void test_sconv(double x) {
    long double ld = (long double)x;
    unsigned char *b = (unsigned char *)&ld;
    long double ld2 = 0;
    unsigned char *c = (unsigned char *)&ld2;
    storef64_tof80(x, &ld2);
    printf("%016llx %02x%02x%02x%02x%02x%02x%02x%02x%02x%02x "
	   "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x %g\n",
	   *(unsigned long long *)&x,
	   b[9], b[8], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0],
	   c[9], c[8], c[7], c[6], c[5], c[4], c[3], c[2], c[1], c[0],
	   x);
}

void test_sconv_from_comp(void) {
    int i;
    double x = 2.7L;
    for (i = 0; i < 80; i++) {
	test_sconv(x);
	x *= 13171.984;
    }
    puts("");
    x = 0.64;
    for (i = 0; i < 80; i++) {
	test_sconv(x);
	x /= 13171.984;
    }
    puts("");
    test_sconv(0.0L);
    test_sconv(-0.0L);
    test_sconv(1.0/0.0);
    test_sconv(-1.0/0.0);
    test_sconv(0.0/0.0);
    test_sconv(-3);
    test_sconv(-1/3.0L);
}

int main(int argc, char **argv) {
    /* test_lconv_from_comp(); */
    test_lconv_from_bits();
    test_sconv_from_comp();
    return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int ite_c(int cond, int then, int elze) {
    return (cond != 0) ? then : elze;
}

/* The inlined assembly here is to force the compiler not to use a
   branch. A conditional move is better for symbolic execution because
   it doesn't cause a path split. */
int ite(int cond, int then, int elze) {
    int result = then;
    asm("test %[cond],%[cond]; cmove %[elze],%[result]"
	: [result] "+r" (result)
	: [cond] "r" (cond), [elze] "rm" (elze));
    return result;
}

int parse_hex(int c) {
    int dec_val = c - '0';
    int is_dec = c >= '0' && c <= '9';
    int uc_val = 10 + c - 'A';
    int is_uc = c >= 'A' && c <= 'F';
    int lc_val = 10 + c - 'a';
    int is_lc = c >= 'a' && c <= 'f';
    return ite(is_dec, dec_val,
	       ite(is_uc, uc_val,
		   ite(is_lc, lc_val, 0)));
}

int parse_hex_byte(char *s) {
    return parse_hex(s[0]) << 4 | parse_hex(s[1]);
}

int parse_hex_short(char *s) {
    return parse_hex_byte(s) << 8 | parse_hex_byte(s + 2);
}

unsigned int parse_hex_int(char *s) {
    return parse_hex_short(s) << 16 | parse_hex_short(s + 4);
}

unsigned long long parse_hex64(char *s) {
    unsigned long long high = parse_hex_int(s);
    unsigned long long low = parse_hex_int(s + 8);
    return high << 32 | low;
}

void print_hex(int d) {
    int dec_val = d + '0';
    int is_dec = d >= 0 && d <= 9;
    int letter_val = d + 'a';
    int is_letter = d >= 10;
    char c = ite(is_dec, dec_val, ite(is_letter, letter_val, '?'));
    putchar(c);
}

void print_mem_hex(unsigned char *p, int len) {
    int i;
    for (i = 0; i < len; i++) {
	unsigned char b = p[i];
	print_hex(b >> 4);
	print_hex(b & 0x0f);
    }
}

double input = 0;

unsigned short pick16(int i) {
    unsigned short s;
    memcpy(&s, (unsigned char *)&input + 2*i, 2);
    return s;
}

int main(int argc, char **argv) {
    short shortened;
    if (argc > 2) {
	fprintf(stderr, "Usage: fp-cast16 <64 bits hex>\n");
	exit(1);
    }
    if (argc == 1) {
	/* leave input unchanged */
    } else if (argc == 2) {
	unsigned long long ull;
	if (strlen(argv[1]) != 16) {
	    fprintf(stderr, "Argument should be 16 hex digits\n");
	    exit(1);
	}
	ull = parse_hex64(argv[1]);
	memcpy(&input, &ull, sizeof(unsigned long long));
    }
    if (pick16(3) != pick16(2)) {
	printf("First two shorts differ.\n");
	return 1;
    }
    if (pick16(2) != pick16(1)) {
	printf("Middle two shorts differ.\n");
	return 2;
    }
    if (pick16(1) != pick16(0)) {
	printf("Final two shorts differ.\n");
	return 3;
    }
    /* printf("Input is %g\n", input); */
    shortened = (short)input;
    /* printf("As short is %d\n", shortened); */
    if (shortened != 42) {
	/* printf("%d != 42\n", shortened); */
	if (shortened < 42) {
	    printf("Too small (0x%04hx)\n", shortened);
	    return 4;
	} else {
	    printf("Too big\n");
	    return 5;
	}
    } else {
	printf("Success with ");
	print_mem_hex((unsigned char *)&input, 8);
	printf("\n");
    }
    return 0;
}

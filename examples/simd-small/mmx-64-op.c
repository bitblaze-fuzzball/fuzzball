#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/mman.h>

/* Sample compilation commands: */

/* gcc-9 -m64 -Wall -g mmx-64-op.c -o mmx-64-op */

/* gcc-9 -I/usr/include/x86_64-linux-gnu -m32 -msse2 -Wall -g mmx-64-op.c -o mmx-64-op-32 */

/* Sample usage:
   ./mmx-64-op 0xef   0x1111222233334444 0x0001000100010001

   ./mmx-64-op 0x100  0x0e0c0a0806040200 0x0301040105900206

   ./mmx-64-op 0x71.4 0x9000800070006000 0x9000800070006000 4
*/

typedef unsigned char uchar_x8 __attribute__ ((vector_size (8)));

/* The memory region used for generated code. code_buf points within
   code_area, and is page-aligned. */
unsigned char code_area[8192];
unsigned char *code_buf;

int parse_hex_digit(char c) {
    if (c >= '0' && c <= '9') {
	return c - '0';
    } else if (c >= 'a' && c <= 'f') {
	return c - 'a' + 10;
    } else if (c >= 'A' && c <= 'F') {
	return c - 'A' + 10;
    } else {
	fprintf(stderr, "Illegal character '%c' in hex\n", c);
	exit(1);
    }
}

uchar_x8 parse_64(const char *s) {
    uchar_x8 r;
    int pos, i;
    if (s[0] == '0' && s[1] == 'x')
	s += 2;
    i = 0;
    for (pos = 7; pos >= 0; pos--) {
	unsigned char byte;
	if (!s[i]) {
	    fprintf(stderr, "Too few hex digits\n");
	    exit(1);
	}
	while (s[i] == ' ' || s[i] == '_')
	    i++;
	byte = parse_hex_digit(s[i]) << 4;
	byte |= parse_hex_digit(s[i + 1]);
	r[pos] = byte;
	i += 2;
    }
    if (s[i] != '\0') {
	fprintf(stderr, "Too many hex digits\n");
	exit(1);
    }
    return r;
}

void print_64(uchar_x8 x) {
    int pos;
    for (pos = 7; pos >= 0; pos--) {
	printf("%02x", x[pos]);
    }
}

int main(int argc, char **argv) {
    long opcode, imm_arg, sub_opcode;
    uchar_x8 arg1, arg2, result;
    unsigned char *cp;
    int res, has_imm = 0, has_subop = 0;
    if (argc != 4 && argc != 5) {
	fprintf(stderr,
		"Usage: mmx-64-op <opcode> <arg1hex> <arg2hex> [<imm>]\n");
	return 1;
    }

    if (strchr(argv[1], '.')) {
	char *dot_ptr;
	opcode = strtol(argv[1], &dot_ptr, 0);
	if (*dot_ptr != '.') {
	    fprintf(stderr,
		    "Opcode+subopcode syntax should be, e.g., 0x71.4\n");
	    return 1;
	}
	sub_opcode = strtol(dot_ptr + 1, 0, 0);
	if (sub_opcode < 0 || sub_opcode > 7) {
	    fprintf(stderr, "Sub-opcode %lx should be between 0 and 7\n",
		    sub_opcode);
	    return 1;
	}
	has_subop = 1;
    } else {
	opcode = strtol(argv[1], 0, 0);
    }
    arg1 = parse_64(argv[2]);
    arg2 = parse_64(argv[3]);
    if (argc == 5) {
	imm_arg = strtol(argv[4], 0, 0);
	if (imm_arg < -128 || imm_arg > 255) {
	    fprintf(stderr, "One-byte immediate 0x%lx out of range\n",
		    imm_arg);
	    return 1;
	}
	has_imm = 1;
    }

    if (opcode < 0 || opcode > 0x1ff) {
	fprintf(stderr, "Opcode 0x%lx out of range\n", opcode);
	exit(1);
    }

    /* Set up a page of memory that is both writable and executable to
       hold the generated code, as an exception to W xor X. */
    code_buf = (unsigned char *)((4095 + (unsigned long)code_area) & ~0xfff);
    res = mprotect(code_buf, 4096, PROT_READ|PROT_WRITE|PROT_EXEC);
    if (res != 0) {
        fprintf(stderr, "mprotect() failed: %s\n", strerror(errno));
        return 1;
    }

    cp = code_buf;
    *cp++ = 0x0f; /* 0x0f is first byte of a two-byte opcode */
    if (opcode < 0x100) {
	*cp++ = opcode;
    } else {
	/* three-byte opcode */
	*cp++ = 0x38;
	*cp++ = (opcode - 0x100);
    }
    if (has_subop) {
	*cp++ = 0xc0 + (sub_opcode << 3) + 1;
    } else {
	*cp++ = 0xca; /* mod/rm: dest = %mm1, src = %mm2 */
    }
    if (has_imm)
	*cp++ = imm_arg;
    *cp++ = 0xc3; /* return */

    asm ("movq %[a1],%%mm1; movq %[a2],%%mm2; "
	 "call *%[code]; movq %%mm1, %[res]"
	 : [res] "=y" (result)
	 : [a1] "y" (arg1), [a2] "y" (arg2), [code] "r" (code_buf)
	 : "mm1", "mm2");

    printf("0x");
    print_64(result);
    printf("\n");
    return 0;
}

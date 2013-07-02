#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "irtoir.h"

using namespace std;

void usage(char *prog)
{
    fprintf(stderr, "Usage: %s -sequential | -random\n", prog);
    fprintf(stderr, "   | -hex-bytes '8b 4d e4' | -hex-shorts 'b001' | -hex-words 'e92d4800'\n");
    fprintf(stderr, "Options for -sequential and -random:\n");
    fprintf(stderr, "  -num N             Do N instructions"
	    " (default: infinity)\n");
    fprintf(stderr, "  -print-interval K  Print every Kth instruction"
	    " (default: all)\n");
    fprintf(stderr, "Options for -random:\n");
    fprintf(stderr, "  -random-seed N     What pseudo-random sequence?\n");
    fprintf(stderr, "General options:\n");
    fprintf(stderr, "  -arm               ARM architecture mode\n");
    fprintf(stderr, "  -thumb             ARM Thumb architecture mode\n");
    fprintf(stderr, "  -x64               x86-64/AMD64/Intel64 mode\n");
    fprintf(stderr, "  -print-vex-ir      Pretty-print VEX's IR\n");
    fprintf(stderr, "  -print-ir          Pretty-print the IR\n");
    fprintf(stderr, "  -disasm            Disassemble in a default format\n");
    fprintf(stderr, "  -disasm-att        Disassemble in AT&T format\n");
    fprintf(stderr, "  -disasm-intel      Disassemble in Intel format\n");
    fprintf(stderr, "  -addr N            Use N as the starting address\n");
}

#define INSN_MAX_LENGTH 16

unsigned int addr = 0x0;
unsigned int thumb_flag = 0;
bool print_this_one;
bool flag_print_ir;
bool flag_disasm_default;
bool flag_disasm_intel;

enum asmir_arch asmir_arch = asmir_arch_x86;
int insn_max_len = 16;
int print_size = 1;

void one_insn(unsigned int addr, unsigned char *bytes, int bytes_len) {
    asm_program_t *asmp =
	byte_insn_to_asmp(asmir_arch, addr | thumb_flag, bytes, bytes_len);
    vine_block_t *vb = asm_addr_to_ir(asmp, addr);
    if (flag_print_ir) {
	vector<Stmt *> *sl = vb->vine_ir;
	for (unsigned int i = 0; i < sl->size(); i++) {
	    printf("%2d: %s\n", i, (*sl)[i]->tostring().c_str());
	}
    }
    if (flag_disasm_default) {
	printf("\t%s\n",
	       string_of_insn(asmp, asmp->functions[addr]->instmap[addr]));
    }
    if (flag_disasm_intel) {
	char *old_o = asmp->disasm_info.disassembler_options;
	char my_opts[] = "intel";
	asmp->disasm_info.disassembler_options = my_opts;
	printf("\t%s\n",
	       string_of_insn(asmp, asmp->functions[addr]->instmap[addr]));
	asmp->disasm_info.disassembler_options = old_o;
    }
    destroy_vine_block(vb);
    free_asm_program(asmp);
}

void print_insn(unsigned char *bytes, int bytes_len) {
    if (print_size == 1) {
	for (int i = 0; i < bytes_len; i++) {
	    printf("%s%02x", (i ? " " : ""), bytes[i]);
	}
    } else if (print_size == 2) {
	for (int i = 0; i < bytes_len; i += 2) {
	    printf("%s%04x", (i ? " " : ""), *(unsigned short *)(bytes + i));
	}
    } else if (print_size == 4) {
	for (int i = 0; i < bytes_len; i += 4) {
	    printf("%s%08x", (i ? " " : ""), *(unsigned int *)(bytes + i));
	}
    } else {
	assert(0);
    }
    printf("\n");
}

void loop(char mode, unsigned long long limit,
	  unsigned long long print_interval) {
    unsigned char bytes[INSN_MAX_LENGTH];
    memset(bytes, 0, INSN_MAX_LENGTH);

    for (unsigned long long count = 0; count < limit; count++) {
	int print_len;
	if (mode == 's') {
	    int i = -1;
	    do {
		i++;
		if (i == insn_max_len)
		    return;
		bytes[i]++;
	    } while (bytes[i] == 0);
	    int len;
	    for (len = insn_max_len - 1; bytes[len] == 0 && len >= 0; len--)
		;
	    len++;
	    print_len = min(len + 5, insn_max_len);
	} else if (mode == 'r') {
	    for (int i = 0; i < insn_max_len; i++) {
		bytes[i] = rand() & 0xff;
	    }
	    print_len = insn_max_len;
	} else {
	    assert(0);
	}
	if (count % print_interval == 0) {
	    print_insn(bytes, print_len);
	    print_this_one = true;
	} else {
	    print_this_one = false;
	}

	one_insn(addr, bytes, print_len);
	addr += print_len;
    }
}

unsigned char *parse_hex_bytes(char *s, int *length) {
    unsigned char *buf = (unsigned char *)malloc(1 + strlen(s));
    unsigned char *q = buf;
    unsigned char a = 0;
    bool even = true;
    for (char *p = s; *p; p++) {
	switch (*p) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    a = (a << 4) | (*p - '0');
	    goto add_nybble;
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	    a = (a << 4) | (*p - 'a' + 10);
	    goto add_nybble;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	    a = (a << 4) | (*p - 'A' + 10);
	add_nybble:
	    even = !even;
	    if (even) {
		*q++ = a;
		a = 0;
	    }
	    break;
	case 'x':
	    if (!even && a == 0) {
		// "0x" is a no-op
		even = !even;
	    } else {
		fprintf(stderr, "Misplaced 'x' in hex byte string\n");
	    }
	    break;
	case ' ':
	    break;
	default:
	    fprintf(stderr, "Unexpected character '%c' in hex byte string\n",
		    *p);
	}
    }
    if (length)
	*length = q - buf;
    return buf;
}

unsigned char *parse_hex_shorts(char *s, int *length) {
    unsigned short *buf = (unsigned short *)malloc(1 + strlen(s));
    unsigned short *q = buf;
    unsigned short a = 0;
    int pos = 0;
    for (char *p = s; *p; p++) {
	switch (*p) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    a = (a << 4) | (*p - '0');
	    goto add_nybble;
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	    a = (a << 4) | (*p - 'a' + 10);
	    goto add_nybble;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	    a = (a << 4) | (*p - 'A' + 10);
	add_nybble:
	    pos++;
	    if (pos == 4) {
		*q++ = a;
		a = 0;
		pos = 0;
	    }
	    break;
	case 'x':
	    if (pos == 1 && a == 0) {
		// "0x" is a no-op
		pos--;
	    } else {
		fprintf(stderr, "Misplaced 'x' in hex short string\n");
	    }
	    break;
	case ' ':
	    break;
	default:
	    fprintf(stderr, "Unexpected character '%c' in hex short string\n",
		    *p);
	}
    }
    if (length)
	*length = sizeof(short) * (q - buf);
    return (unsigned char *)buf;
}

unsigned char *parse_hex_words(char *s, int *length) {
    unsigned int *buf = (unsigned int *)malloc(1 + strlen(s));
    unsigned int *q = buf;
    unsigned int a = 0;
    int pos = 0;
    for (char *p = s; *p; p++) {
	switch (*p) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    a = (a << 4) | (*p - '0');
	    goto add_nybble;
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	    a = (a << 4) | (*p - 'a' + 10);
	    goto add_nybble;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	    a = (a << 4) | (*p - 'A' + 10);
	add_nybble:
	    pos++;
	    if (pos == 8) {
		*q++ = a;
		a = 0;
		pos = 0;
	    }
	    break;
	case 'x':
	    if (pos == 1 && a == 0) {
		// "0x" is a no-op
		pos--;
	    } else {
		fprintf(stderr, "Misplaced 'x' in hex word string\n");
	    }
	    break;
	case ' ':
	    break;
	default:
	    fprintf(stderr, "Unexpected character '%c' in hex word string\n",
		    *p);
	}
    }
    if (length)
	*length = sizeof(int) * (q - buf);
    return (unsigned char *)buf;
}

int main(int argc, char *argv[])
{
    unsigned long long limit = -1LL;
    unsigned long long print_interval = 1LL;
    char mode = 0;
    unsigned char *bytes = 0;
    int bytes_len;
    if (argc <= 1){
	usage(argv[0]);
	return -1;
    }
    for (int i = 1; i < argc; i++) {
	if (!strcmp(argv[i], "-sequential")) {
	    mode = 's';
	} else if (!strcmp(argv[i], "-random")) {
	    mode = 'r';
	} else if (!strcmp(argv[i], "-hex-bytes")) {
	    mode = 'b';
	    assert(i + 1 < argc);
	    bytes = parse_hex_bytes(argv[i + 1], &bytes_len);
	    i++;
	} else if (!strcmp(argv[i], "-hex-shorts")) {
	    mode = 'b';
	    assert(i + 1 < argc);
	    bytes = parse_hex_shorts(argv[i + 1], &bytes_len);
	    i++;
	} else if (!strcmp(argv[i], "-hex-words")) {
	    mode = 'b';
	    assert(i + 1 < argc);
	    bytes = parse_hex_words(argv[i + 1], &bytes_len);
	    i++;
	} else if (!strcmp(argv[i], "-print-ir")) {
	    flag_print_ir = true;
	} else if (!strcmp(argv[i], "-print-vex-ir")) {
	    debug_on("vex");
	} else if (!strcmp(argv[i], "-arm")) {
	    asmir_arch = asmir_arch_arm;
	    insn_max_len = 4;
	    print_size = 4;
	} else if (!strcmp(argv[i], "-thumb")) {
	    asmir_arch = asmir_arch_arm;
	    insn_max_len = 4;
	    print_size = 2;
	    thumb_flag = 1;
	} else if (!strcmp(argv[i], "-x64")) {
	    asmir_arch = asmir_arch_x64;
	} else if (!strcmp(argv[i], "-disasm-att")) {
	    flag_disasm_default = true;
	} else if (!strcmp(argv[i], "-disasm")) {
	    flag_disasm_default = true;
	} else if (!strcmp(argv[i], "-disasm-intel")) {
	    flag_disasm_intel = true;
	} else if (!strcmp(argv[i], "-num")) {
	    assert(i + 1 < argc);
	    limit = strtoull(argv[i + 1], 0, 0);
	    i++;
	} else if (!strcmp(argv[i], "-addr")) {
	    assert(i + 1 < argc);
	    addr = strtoul(argv[i + 1], 0, 0);
	    if ((addr & (print_size - 1)) != 0) {
		fprintf(stderr, "Argument to -addr is misaligned\n");	      
	    }
	    i++;
	} else if (!strcmp(argv[i], "-random-seed")) {
	    assert(i + 1 < argc);
	    srand(strtoul(argv[i + 1], 0, 0));
	    i++;
	} else if (!strcmp(argv[i], "-print-interval")) {
	    assert(i + 1 < argc);
	    print_interval = strtoull(argv[i + 1], 0, 0);
	    i++;
	} else {
	    fprintf(stderr, "Unrecognized option %s\n", argv[i]);
	    usage(argv[0]);
	    return -1;
	}
    }

    translate_init();

    switch (mode) {
    case 's':
    case 'r':
	loop(mode, limit, print_interval);
	break;
    case 'b':
	print_insn(bytes, bytes_len);
	one_insn(addr, bytes, bytes_len);
	free(bytes);
	break;
    }
    
    return 0;
}
    

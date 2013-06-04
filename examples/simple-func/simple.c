#include <stdio.h>
#include <stdlib.h>

int simple(int x) {
    int path = 0;
    if (x & 1) {
	printf("x is odd\n");
	path++;
    } else {
	printf("x is even\n");
    }
    path <<= 1;
    if (x % 11 == 0) {
	printf("x is a multiple of 11\n");
	path++;
    } else {
	printf("x is not a multiple of 11\n");
    }
    printf("Took path %d\n", path);
}

int main(int argc, char **argv) {
    int input;
    if (argc != 2) {
	fprintf(stderr, "Usage: simple <integer>\n");
	exit(-1);
    }
    input = atoi(argv[1]);
    simple(input + 10);
    return 0;
}

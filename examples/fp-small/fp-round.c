#include <stdio.h>
#include <stdlib.h>

int n;

int main(int argc, char **argv) {
    if (argc == 2) {
	n = atoi(argv[1]);
    } else if (argc == 1) {
	/* For FuzzBALL's use */
    } else {
	fprintf(stderr, "Usage: fp-round n\n");
	exit(1);
    }
    float f = n;
    int n2 = f;
    if (n == n2) {
	/*printf("Unchanged: %x = %x\n", n, n2);*/
	printf("Unchanged\n");
    } else if (n2 < n) {
	/*printf("Shrunk: %x > %x\n", n, n2);*/
	printf("Shrunk\n");
    } else {
	/*printf("Grew: %x < %x\n", n, n2);*/
	printf("Grew\n");
    }
    return 0;
}
